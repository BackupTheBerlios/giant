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
--  $RCSfile: giant-make_room_dialog.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/23 10:41:10 $
--

with Ada.IO_Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

with Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Box;
with Gtk.Label;
with Gtk.Spin_Button;

with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Make_Room_Dialog is

   function Can_Hide
     (Dialog : access Make_Room_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then
         -- the okay button was pressed
         -- FIX: make room
         null;
      end if;

      return True;
   end Can_Hide;

   procedure Create
     (Dialog : out Make_Room_Dialog_Access)
   is
   begin
      Dialog := new Make_Room_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Make_Room_Dialog_Record'class)
   is
      Pixel_Button : Gtk.Spin_Button.Gtk_Spin_Button;
      Label : Gtk.Label.Gtk_Label;
      Center_Box : Gtk.Box.Gtk_Vbox;
      Pixel_Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Initialize (Dialog, -"Make Room",
                                 Default_Dialog.Button_Okay_Cancel);

      --  center box
      Center_Box := Get_Center_Box (Dialog);
      Label := New_Label(-"Create an empty space by shifting away all nodes from the selected point.");
      Gtk.Label.Set_Line_Wrap (Label, True);

      Gtk.Box.Pack_Start (Center_Box, Label, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      --  spin button
      Gtk.Adjustment.Gtk_New (Dialog.Pixel_Adjustment,
                              Value => 0.0, Lower => 0.0,
                              Upper => 100.0, Step_Increment => 1.0,
                              Page_Increment => 1.0, Page_Size => 1.0);
      Gtk.Spin_Button.Gtk_New (Pixel_Button, Dialog.Pixel_Adjustment,
                               Climb_Rate => 1.0, The_Digits => 0);
      Gtk.Spin_Button.Set_Numeric (Pixel_Button, True);
      Gtk.Spin_Button.Set_Value (Pixel_Button, DEFAULT_VALUE);

      --  pixel label
      Gtk.Box.Gtk_New_Hbox (Pixel_Box);
      Gtk.Box.Pack_Start (Center_Box, Pixel_Box, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      Gtk.Box.Pack_Start (Pixel_Box, Pixel_Button, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Pixel_Box, New_Label(-"Pixels"), Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      Set_Default (Dialog, Pixel_Button);
   end;

end Giant.Make_Room_Dialog;
