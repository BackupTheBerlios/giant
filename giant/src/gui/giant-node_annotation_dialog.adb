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
--  $RCSfile: giant-node_annotation_dialog.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/12 13:58:14 $
--

with Glib;
with Gtk.Button;
with Gtk.Scrolled_Window;
with Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;

with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Node_Annotation_Dialog is

   procedure Set_Text
     (Dialog   : access Node_Annotation_Dialog_Record'Class;
      Text : String)
   is
      Position : Glib.Gint := 0;
   begin
      Gtk.Text.Delete_Text (Dialog.Text_Area);
      Gtk.Text.Insert_Text (Dialog.Text_Area, Text, Position);
   end Set_Text;

   procedure On_Delete_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Annotation_Dialog_Access;
   begin
      Dialog := Node_Annotation_Dialog_Access (Gtk.Widget.Get_Toplevel
                                   (Gtk.Widget.Gtk_Widget (Source)));
      -- FIX: delete annotation
   end On_Delete_Button_Clicked;

   function Can_Hide
     (Dialog : access Node_Annotation_Dialog_Record)
     return Boolean
   is
      --  the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
      use Default_Dialog;
   begin
      Response := Default_Dialog.Get_Response (Dialog);
      if (Default_Dialog.Get_Response (Dialog)
          = Default_Dialog.Response_Okay) then
         -- the okay button was pressed
         -- FIX: save annotation
         null;
      end if;

      return True;
   end Can_Hide;

   procedure Create
     (Dialog :    out Node_Annotation_Dialog_Access;
      Node   : in     Graph_Lib.Node_Id)
   is
   begin
      Dialog := new Node_Annotation_Dialog_Record;
      Dialog.Node := Node;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Node_Annotation_Dialog_Record'class)
   is
      Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"Annotate Node",
                                 Default_Dialog.Button_Okay_Cancel);

      -- text area
      Gtk.Text.Gtk_New (Dialog.Text_Area);
      Gtk.Text.Set_Editable (Dialog.Text_Area, True);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Dialog.Text_Area);

      Default_Dialog.Set_Center_Widget (Dialog, Scrolled_Window);
      Set_Default (Dialog, Dialog.Text_Area);

      -- FIX: set node annotation
      Set_Text (Dialog, "Annotation");

      -- buttons
      Default_Dialog.Add (Dialog,
                          New_Button (-"Delete",
                                      On_Delete_Button_Clicked'Access));
   end;

end Giant.Node_Annotation_Dialog;
