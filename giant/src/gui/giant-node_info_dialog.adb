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
--  $RCSfile: giant-node_info_dialog.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/06/23 16:15:41 $
--

with Interfaces.C.Strings;

with Glib;
with Gtk.Box;
with Gtk.Button;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Enums;
with Gtkada.Types;

with Giant.Gui_Utils;
with Giant.Node_Annotation_Dialog;

package body Giant.Node_Info_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Annotate_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access
        := Node_Info_Dialog_Access (Gtk.Widget.Get_Toplevel
                                    (Gtk.Widget.Gtk_Widget (Source)));
   begin
      Node_Annotation_Dialog.Show (Dialog.Node);
   end On_Annotate_Button_Clicked;

   procedure On_Pick_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access
        := Node_Info_Dialog_Access (Gtk.Widget.Get_Toplevel
                                    (Gtk.Widget.Gtk_Widget (Source)));
   begin
      -- FIX: pick node
      null;
   end On_Pick_Button_Clicked;

   procedure On_Update_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access
        := Node_Info_Dialog_Access (Gtk.Widget.Get_Toplevel
                                    (Gtk.Widget.Gtk_Widget (Source)));
   begin
      Set_Node (Dialog, Dialog.Node);
   end On_Update_Button_Clicked;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog :    out Node_Info_Dialog_Access)
   is
   begin
      Dialog := new Node_Info_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Node_Info_Dialog_Record'class)
   is
      use Giant.Gui_Utils;

      Center_Box : Gtk.Box.Gtk_Vbox;
      Table : Gtk.Table.Gtk_Table;
      Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Node Info",
                                 Default_Dialog.Button_Close);

      --  vbox
      Center_Box := Get_Center_Box (Dialog);

      --  node
      Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Gtk.Table.Set_Col_Spacings (Table, Glib.Guint
                                  (DEFAULT_SPACING));
      Gtk.Table.Set_Border_Width (Table, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Center_Box, Add_Frame (Table, -"Node"),
                          expand => False, Fill => False,
                          Padding => DEFAULT_SPACING);

      Dialog.ID_Label := New_Label ("");
      Add_Row_Labels (Table, Row, New_Label (-"ID"), Dialog.ID_Label);

      Dialog.Type_Label := New_Label ("");
      Add_Row_Labels (Table, Row, New_Label (-"Type"), Dialog.Type_Label);

      --  attributes
      Clists.Create (Dialog.Attribute_List, 2);
      Clists.Set_Column_Title (Dialog.Attribute_List, 0, -"Attribute");
      Clists.Set_Column_Title (Dialog.Attribute_List, 1, -"Value");
      Gtk.Box.Pack_Start (Center_Box,
                          Add_Scrollbars (Dialog.Attribute_List),
                          expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

      --  buttons
      Add_Button (Dialog,
                  New_Button (-"Annotate", On_Annotate_Button_Clicked'Access));
      Add_Button (Dialog,
                  New_Button (-"Update", On_Update_Button_Clicked'Access));
      Add_Button (Dialog,
                  New_Button (-"Pick", On_Pick_Button_Clicked'Access));
   end;

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Node_Info_Dialog_Record)
     return Boolean
   is
   begin
      -- FIX: abort a possible pick action
      return True;
   end Can_Hide;

   procedure Set_Node
     (Dialog : access Node_Info_Dialog_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
   is
      Row : Glib.Gint;
      Row_Data : Gtkada.Types.Chars_Ptr_Array (0 .. 1);
      Iterator : Graph_Lib.Node_Attribute_Iterator;
      Attribute : Graph_Lib.Node_Attribute_Id;
      Width : Glib.Gint;
   begin
      Dialog.Node := Node;

      Gtk.Label.Set_Text (Dialog.ID_Label, Graph_Lib.Node_Id_Image (Node));
      Gtk.Label.Set_Text (Dialog.Type_Label,
                          Graph_Lib.Get_Node_Class_Tag
                          (Graph_Lib.Get_Node_Class_Id (Node)));

      --  set attributes
      Clists.Clear (Dialog.Attribute_List);
      Iterator := Graph_Lib.Make_Attribute_Iterator (Node);
      while (Graph_Lib.More (Iterator)) loop
         Graph_Lib.Next (Iterator, Attribute);
         Row_Data (0) := Interfaces.C.Strings.New_String
           (Graph_Lib.Convert_Node_Attribute_Id_To_Name (Attribute));
         declare
         begin
            Row_Data (1) := Interfaces.C.Strings.New_String
              (Graph_Lib.Get_Node_Attribute_Value_As_String (Node,
                                                             Attribute));
         exception
           when Giant.Graph_Lib.Node_Does_Not_Exist =>
              Row_Data (1) := Interfaces.C.Strings.New_String (-"*Not Found*");
         end;
         Row := Clists.Append (Dialog.Attribute_List, Row_Data);
         Gtkada.Types.Free (Row_Data);
      end loop;

      --  resize columns
      Width := Clists.Columns_Autosize (Dialog.Attribute_List);
   end Set_Node;
end Giant.Node_Info_Dialog;
