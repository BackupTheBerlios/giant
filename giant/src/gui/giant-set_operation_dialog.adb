------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-set_operation_dialog.adb,v $, $Revision: 1.12 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Ada.Strings.Unbounded;

with Glib;
with Gtk.Box;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Window;

with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Dialogs;
with Giant.Projects;
with Giant.Gui_Utils;

package body Giant.Set_Operation_Dialog is

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog :    out Set_Operation_Dialog_Access;
      Items  : in     Gtk.Enums.String_List.Glist)
   is
   begin
      Dialog := new Set_Operation_Dialog_Record;
      Initialize (Dialog, Items);
   end Create;

   procedure Initialize
     (Dialog : access Set_Operation_Dialog_Record'Class;
      Items  : in     Gtk.Enums.String_List.Glist)
   is
      use Giant.Gui_Utils;
      use type Glib.Guint;

      Table : Gtk.Table.Gtk_Table;
      Operations : Gtk.Enums.String_List.Glist;
      Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Set Operation",
                                 Default_Dialog.Button_Okay_Cancel);

      Gtk.Table.Gtk_New (Table, Rows => 5, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Col_Spacings (Table, DEFAULT_SPACING);
      Set_Center_Widget (Dialog, Table);

      --  left source
      Gtk.Combo.Gtk_New (Dialog.Left_Source);
      if (Gtk.Enums.String_List.Length (Items) > 0) then
         Gtk.Combo.Set_Popdown_Strings (Dialog.Left_Source, Items);
      end if;
      Add_Row (Table, Row, New_Label (-"Left Source"), Dialog.Left_Source);

      --  operation
      Gtk.Enums.String_List.Append (Operations, -"Difference");
      Gtk.Enums.String_List.Append (Operations, -"Intersection");
      Gtk.Enums.String_List.Append (Operations, -"Union");

      Gtk.Combo.Gtk_New (Dialog.Operation);
      Gtk.Combo.Set_Popdown_Strings (Dialog.Operation, Operations);
      Gtk.Combo.Set_Value_In_List
        (Combo_Box   => Dialog.Operation,
         Val         => True,
         Ok_If_Empty => False);
      Add_Row (Table, Row, New_Label (-"Operation"), Dialog.Operation);

      --  right source
      Gtk.Combo.Gtk_New (Dialog.Right_Source);
      if (Gtk.Enums.String_List.Length (Items) > 0) then
         Gtk.Combo.Set_Popdown_Strings (Dialog.Right_Source, Items);
      end if;
      Add_Row (Table, Row, New_Label (-"Right Source"), Dialog.Right_Source);

      --  separator
      Gtk.Table.Attach (Table, New_Hseperator,
                        Left_Attach => 1, Right_Attach => 2,
                        Top_Attach => 3, Bottom_Attach => 4);
      Row := Row + 1;

      --  target
      Gtk.Gentry.Gtk_New (Dialog.Target);
      Add_Row (Table, Row, New_Label (-"Target"), Dialog.Target);
   end;

   ---------------------------------------------------------------------------
   --  Inspectors
   ---------------------------------------------------------------------------

   function Get_Left_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Text (Gtk.Combo.Get_Entry (Dialog.Left_Source));
   end;

   function Get_Right_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Text (Gtk.Combo.Get_Entry (Dialog.Right_Source));
   end;

   function Get_Operation
     (Dialog : access Set_Operation_Dialog_Record)
     return Operation_Type
   is
      Operation : String
        := Gtk.Gentry.Get_Text (Gtk.Combo.Get_Entry (Dialog.Operation));
   begin
      if (Operation = -"Difference") then
         return Difference;
      elsif (Operation = -"Intersection") then
         return Intersection;
      elsif (Operation = -"Union") then
         return Union;
      end if;

      raise Invalid_Operation_Entered;
   end;

   function Get_Target
     (Dialog : access Set_Operation_Dialog_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Text (Dialog.Target);
   end;

   function Validate
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean
   is
      Operation : Operation_Type;
   begin
      if (Get_Target (Dialog) = "") then
         Controller.Show_Error (-"Please provide a name for the target.");
         return False;
      end if;

      Operation := Get_Operation (Dialog);

      return True;
   exception
     when Invalid_Operation_Entered =>
        Controller.Show_Error (-"Please select a valid operation.");
        return False;
   end Validate;

end Giant.Set_Operation_Dialog;

