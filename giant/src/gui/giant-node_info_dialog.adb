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
--  $RCSfile: giant-node_info_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/20 16:47:35 $
--

with Glib;
with Gtk.Box;
with Gtk.Button;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;

with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Node_Info_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Pick_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access;
   begin
      Dialog := Node_Info_Dialog_Access (Gtk.Widget.Get_Toplevel
                                   (Gtk.Widget.Gtk_Widget (Source)));
      -- FIX: delete info
   end On_Pick_Button_Clicked;

   procedure On_Update_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Info_Dialog_Access;
   begin
      Dialog := Node_Info_Dialog_Access (Gtk.Widget.Get_Toplevel
                                   (Gtk.Widget.Gtk_Widget (Source)));
      -- FIX: delete info
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
	  Center_Box : Gtk.Box.Gtk_Vbox;
	  Table : Gtk.Table.Gtk_Table;
	  Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Node Info",
                                 Default_Dialog.Button_Close);
	  
	  --  vbox
	  Center_Box := Default_Dialog.Get_Center_Box (Dialog);
	  
	  --  node
	  Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Gtk.Table.Set_Col_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
	  Gtk.Box.Pack_Start (Center_Box, Table, expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);
	  
	  Dialog.ID_Label := New_Label ("");
	  Add_Row (Table, Row, New_Label (-"ID"), Dialog.ID_Label);
	  
	  Dialog.Type_Label := New_Label ("");
	  Add_Row (Table, Row, New_Label (-"ID"), Dialog.Type_Label);
	  
      --  buttons
      Default_Dialog.Add (Dialog, New_Button (-"Update",
											  On_Update_Button_Clicked'Access));
	  Default_Dialog.Add (Dialog, New_Button (-"Pick",
											  On_Pick_Button_Clicked'Access));

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
	  Node	 : in     Graph_Lib.Node_Id)
   is
   begin
	  Gtk.Label.Set_Text (Dialog.ID_Label, Graph_Lib.Node_Id_Image (Node));
--  	  Gtk.Label.Set_Text (Dialog.Type_Label, 
--  						  Graph_Lib.Get_Node_Class_Id (Node)
   end Set_Node;
end Giant.Node_Info_Dialog;
