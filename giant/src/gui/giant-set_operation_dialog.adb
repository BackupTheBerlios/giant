------------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-set_operation_dialog.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/19 16:38:06 $
--

with Ada.Strings.Unbounded;

with Glib;
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Misc;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Window;

with String_Lists;

with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Projects;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Set_Operation_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      -- the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
   begin
      Response := Default_Dialog.Get_Response (Dialog);

      if (Default_Dialog.Get_Response (Dialog)
          = Default_Dialog.Response_Okay) then
         -- run the operation
         null;
      end if;

      return True;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog : out Set_Operation_Dialog_Access)
   is
   begin
      Dialog := new Set_Operation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   function Get_Subgraphs
     return String_List.Glist
   is
      Source : String_Lists.List;
      Target : String_List.Glist;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  initialize main window data
      Source := Projects.Get_All_Subgraphs (Controller.Get_Project);
      Iterator := String_Lists.MakeListIter (Source);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         String_List.Append (Target, Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (Source);

      return Target;
   end;

   procedure Add_Row
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in     Glib.Guint;
      Left  : access Gtk.Misc.Gtk_Misc_Record'Class;
      Right : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Glib.Guint;
   begin
      --  right align
      Gtk.Misc.Set_Alignment (Left, 1.0, 0.5);

      Gtk.Table.Attach (Table, Left,
                        Left_Attach => 0, Right_Attach => 1,
                        Top_Attach => Row, Bottom_Attach => Row + 1);
      Gtk.Table.Attach (Table, Right,
                        Left_Attach => 1, Right_Attach => 2,
                        Top_Attach => Row, Bottom_Attach => Row + 1);
   end;

   procedure Initialize
     (Dialog  : access Set_Operation_Dialog_Record'Class)
   is
      use type Glib.Guint;

      Table : Gtk.Table.Gtk_Table;
      Operations : String_List.Glist;
      Subgraphs : String_List.Glist := Get_Subgraphs;
   begin
      Default_Dialog.Initialize (Dialog, -"Set Operation",
                                 Default_Dialog.Button_Okay_Cancel);

      Gtk.Table.Gtk_New (Table, Rows => 5, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Gtk.Table.Set_Col_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Default_Dialog.Set_Center_Widget (Dialog, Table);

      --  left source
      Gtk.Combo.Gtk_New (Dialog.Left_Source);
      if (String_List.Length (Subgraphs) > 0) then
         Gtk.Combo.Set_Popdown_Strings (Dialog.Left_Source, Subgraphs);
      end if;
      Add_Row (Table, 0, New_Label (-"Left Source"), Dialog.Left_Source);

      --  operation
      String_List.Append (Operations, -"Difference");
      String_List.Append (Operations, -"Intersection");
      String_List.Append (Operations, -"Union");

      Gtk.Combo.Gtk_New (Dialog.Operation);
      Gtk.Combo.Set_Popdown_Strings (Dialog.Operation, Operations);
      Gtk.Combo.Set_Value_In_List (Dialog.Operation, 0, Ok_If_Empty => False);
      Add_Row (Table, 1, New_Label (-"Operation"), Dialog.Operation);

      --  left source
      Gtk.Combo.Gtk_New (Dialog.Left_Source);
      if (String_List.Length (Subgraphs) > 0) then
         Gtk.Combo.Set_Popdown_Strings (Dialog.Left_Source, Subgraphs);
      end if;
      Add_Row (Table, 2, New_Label (-"Left Source"), Dialog.Left_Source);

      --  separator
      Gtk.Table.Attach (Table, New_Hseperator,
                        Left_Attach => 1, Right_Attach => 2,
                        Top_Attach => 3, Bottom_Attach => 4);

      --  target
      Gtk.Gentry.Gtk_New (Dialog.Target);
      Add_Row (Table, 4, New_Label (-"Target"), Dialog.Target);
   end;

end Giant.Set_Operation_Dialog;

