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
--  $RCSfile: giant-layout_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/05 20:13:42 $
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

package body Giant.Layout_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog		 :    out Layout_Dialog_Access;
	  Window_Name	 : in     String;
	  Selection_Name : in     String);
   is
   begin
      Dialog := new Layout_Dialog_Record;
      Initialize (Dialog, Window_Name, Selection_Name);
   end Create;

   procedure Initialize
     (Dialog		 : access Layout_Dialog_Record'Class;
	  Window_Name	 : in     String;
	  Selection_Name : in     String);
   is
      use Giant.Gui_Utils;

      Center_Box : Gtk.Box.Gtk_Vbox;
      Table : Gtk.Table.Gtk_Table;
      Row : Glib.Guint := 0;
   begin
      Default_Dialog.Initialize (Dialog, -"Apply Layout",
                                 Default_Dialog.Button_Okay_Cancel);

	  Dialog.Window_Name
		:= Ada.String.Unbounded.To_Unbounded_String (Window_Name);
	  Dialog.Selection_Name
		:= Ada.String.Unbounded.To_Unbounded_String (Selection_Name);

      --  vbox
      Center_Box := Get_Center_Box (Dialog);

      --  node
      Gtk.Table.Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Gtk.Table.Set_Row_Spacings (Table, Glib.Guint (DEFAULT_SPACING));
      Gtk.Table.Set_Col_Spacings (Table, Glib.Guint
                                  (DEFAULT_SPACING));
      Gtk.Table.Set_Border_Width (Table, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Center_Box, Add_Frame (Table, ""),
                          expand => False, Fill => False,
                          Padding => DEFAULT_SPACING);

      Add_Row_Labels (Table, Row, New_Label (-"Window"), 
					  New_Label (Window_Name));
      Add_Row_Labels (Table, Row, New_Label (-"Selection"), 
					  New_Label (Selection_Name));

	  --  notebook
      Gtk.Notebook.Gtk_New (Dialog.Layout_Notebook);
      Gtk.Notebook.Popup_Enable (Dialog.Layout_Notebook);
      Gtk.Box.Pack_Start (Center_Box, Dialog.Layout_Notebook,
                          Expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

--        Gtk.Notebook.Append_Page_Menu (Notebook, Label,
--                                       New_Label (-"About"),
--                                       New_Label (-"About"));
   end;

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Layout_Dialog_Record)
     return Boolean
   is
	  use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then
		 -- FIX: apply layout
	  end if;

	  return True;
   end Can_Hide;

   procedure Show
	 (Window_Name	 : in String;
	  Selection_Name : in String;
	  Position		 : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d)
   is
	  Dialog : access Layout_Dialog_Access;
   begin
	  Create (Dialog, Dialog Window_Name, Selection_Name);
	  Dialog.Position := Position;
	  Show_Modal (Dialog);
	  Destroy (Dialog);
   end Show;

end Giant.Layout_Dialog;
