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
--  $RCSfile: giant-layout_dialog-widgets.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $

with Glib;
with Gtk.Enums;
with Gtk.Table;

with Lists;
with String_Lists;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Graph_Lib;
with Giant.Gui_Utils;
with Giant.Layout_Factory;

package body Giant.Layout_Dialog.Widgets is

   ---------------------------------------------------------------------------
   --  Matrix Layout
   ---------------------------------------------------------------------------

   function Create_Matrix
     return Matrix_Layout_Container_Access
   is
      Container : Matrix_Layout_Container_Access;
   begin
      Container := new Matrix_Layout_Container_Record;

      Gtk.Box.Gtk_New_Vbox (Container.Widget);
      Gtk.Box.Add (Container.Widget, Gui_Utils.New_Label
                   (-"The matrix layout does not require configuration."));

      return Container;
   end Create_Matrix;

   function Get_Widget
     (Container : access Matrix_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget
   is
   begin
      return Gtk.Widget.Gtk_Widget (Container.Widget);
   end;

   function Get_Display_Name
     (Container : access Matrix_Layout_Container_Record)
     return String
   is
   begin
      return -"Matrix";
   end Get_Display_Name;

   function Get_Layout_Name
     (Container : access Matrix_Layout_Container_Record)
     return String
   is
   begin
      return "matrix";
   end Get_Layout_Name;

   function Get_Layout_Parameters
     (Container : access Matrix_Layout_Container_Record)
     return String
   is
   begin
      return "";
   end Get_Layout_Parameters;

   ---------------------------------------------------------------------------
   --  Tree Layout
   ---------------------------------------------------------------------------

   procedure Update_Class
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
   end Update_Class;

   function Create_Tree
     return Tree_Layout_Container_Access
   is
      use Giant.Gui_Utils;

      Container : Tree_Layout_Container_Access;
      List      : String_Lists.List;
      Iterator  : String_Lists.ListIter;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Container := new Tree_Layout_Container_Record;

      Gtk.Box.Gtk_New_Vbox (Container.Widget);
      Gtk.Box.Pack_Start (Container.Widget,
                          Gui_Utils.New_Label (-"Root Node ID:"),
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);
      Gtk.Gentry.Gtk_New (Container.Root_Node);
      Gtk.Box.Pack_Start (Container.Widget, Container.Root_Node,
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      Gtk.Box.Pack_Start (Container.Widget,
                          Gui_Utils.New_Label
                          (-"Please select the class sets."),
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      String_Clists.Create (Container.Class_Set_List, 1, Update_Class'Access);
      String_Clists.Set_Column_Title (Container.Class_Set_List, 0, -"Class");
      String_Clists.Set_Selection_Mode (Container.Class_Set_List,
                                        Gtk.Enums.Selection_Multiple);

      List := Config.Class_Sets.Get_All_Existing_Class_Sets;
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         String_Clists.Add (Container.Class_Set_List,
                            Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);
      Gtk.Box.Pack_Start (Container.Widget, Container.Class_Set_List,
                          Expand => True, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      Gtk.Check_Button.Gtk_New
        (Container.Check, -"Process edges reverse");
      Gtk.Box.Pack_Start (Container.Widget,
                          Container.Check,
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      return Container;
   end Create_Tree;

   function Get_Widget
     (Container : access Tree_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget
   is
   begin
      return Gtk.Widget.Gtk_Widget (Container.Widget);
   end;

   function Get_Display_Name
     (Container : access Tree_Layout_Container_Record)
     return String
   is
   begin
      return -"Tree";
   end Get_Display_Name;

   function Get_Layout_Name
     (Container : access Tree_Layout_Container_Record)
     return String
   is
   begin
      return "tree";
   end Get_Layout_Name;

   function Get_Layout_Parameters
     (Container : access Tree_Layout_Container_Record)
     return String
   is
      use type Gtk.Enums.Gint_List.Glist;
      Classes : Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Selection : Gtk.Enums.Gint_List.Glist
        := Gui_Utils.String_Clists.Get_Selection (Container.Class_Set_List);
      Row : Glib.Gint;

   begin
      while (Selection /= Gtk.Enums.Gint_List.Null_List) loop
         Row := Gtk.Enums.Gint_List.Get_Data (Selection);
         Ada.Strings.Unbounded.Append
           (Classes, Gui_Utils.String_Clists.Data.Get
            (Container.Class_Set_List, Row)
            & ",");
         Selection := Gtk.Enums.Gint_List.Next (Selection);
      end loop;

      declare
         Res : String := Gtk.Gentry.Get_Text (Container.Root_Node) & ";" &
           Ada.Strings.Unbounded.To_String (Classes);
      begin
         if Gtk.Check_Button.Get_Active (Container.Check) then
            return Res & ";" & Layout_Factory.Process_Edges_Reverse;
         else
            return Res;
         end if;
      end;
   end Get_Layout_Parameters;

   ---------------------------------------------------------------------------
   --  Other Layout
   ---------------------------------------------------------------------------

   function Create_Other
     return Other_Layout_Container_Access
   is
      use Giant.Gui_Utils;

      Container : Other_Layout_Container_Access;
      Table : Gtk.Table.Gtk_Table;
      Row : Glib.Guint := 0;
   begin
      Container := new Other_Layout_Container_Record;

      Gtk.Box.Gtk_New_Vbox (Container.Widget);

      Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Col_Spacings (Table, DEFAULT_SPACING);
      Gtk.Table.Set_Border_Width (Table, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Container.Widget, Table,
                          expand => False, Fill => False,
                          Padding => DEFAULT_SPACING);

      Gtk.Gentry.Gtk_New (Container.Layout_Name);
      Add_Row (Table, Row, New_Label (-"Layout Algorithm"),
               Container.Layout_Name);

      Gtk.Gentry.Gtk_New (Container.Parameters);
      Add_Row (Table, Row, New_Label (-"Parameters"),
               Container.Parameters);
      return Container;
   end Create_Other;

   function Get_Widget
     (Container : access Other_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget
   is
   begin
      return Gtk.Widget.Gtk_Widget (Container.Widget);
   end;

   function Get_Display_Name
     (Container : access Other_Layout_Container_Record)
     return String
   is
   begin
      return -"Other";
   end Get_Display_Name;

   function Get_Layout_Name
     (Container : access Other_Layout_Container_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Text (Container.Layout_Name);
   end Get_Layout_Name;

   function Get_Layout_Parameters
     (Container : access Other_Layout_Container_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Text (Container.Parameters);
   end Get_Layout_Parameters;

end Giant.Layout_Dialog.Widgets;
