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
--  $RCSfile: giant-layout_dialog-widgets.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/08 16:07:32 $

with Glib;
with Gtk.Table;

with Lists;
with String_Lists;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Gui_Utils;

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
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Container := new Tree_Layout_Container_Record;

      Gtk.Box.Gtk_New_Vbox (Container.Widget);
      Gtk.Box.Pack_Start (Container.Widget,
                          Gui_Utils.New_Label
                          (-"Please select the class sets."),
                          Expand => False, Fill => True,
                          Padding => Gui_Utils.DEFAULT_SPACING);

      String_Clists.Create (Container.Class_Set_List, 1, Update_Class'Access);
      String_Clists.Set_Column_Title (Container.Class_Set_List, 0, -"Class");

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
   begin
      return "";
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
      Gtk.Table.Set_Row_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Gtk.Table.Set_Col_Spacings (Table, Glib.Guint
                                  (DEFAULT_SPACING));
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
      return Gtk.Gentry.Get_Chars (Container.Layout_Name);
   end Get_Layout_Name;

   function Get_Layout_Parameters
     (Container : access Other_Layout_Container_Record)
     return String
   is
   begin
      return Gtk.Gentry.Get_Chars (Container.Parameters);
   end Get_Layout_Parameters;

end Giant.Layout_Dialog.Widgets;
