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
--  $RCSfile: giant-progress_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/02 01:04:18 $
--

with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Progress_Bar;
with Gtk.Window;

with Giant.Default_Dialog;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Progress_Dialog is

   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
     return Boolean
   is
      -- the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
      use Default_Dialog;
   begin
      Response := Default_Dialog.Get_Response (Dialog);

      if (Default_Dialog.Get_Response (Dialog)
          = Default_Dialog.Response_Cancel) then
		 -- the cancel button was pressed
		 return True;
      end if;

      return False;
   end Can_Hide;

   procedure Create
     (Dialog :    out Progress_Dialog_Access;
	  Title	 : in     String)
   is
   begin
      Dialog := new Progress_Dialog_Record;
      Initialize (Dialog, Title);
   end Create;

   procedure Initialize
     (Dialog : access Progress_Dialog_Record'Class;
	  Title	 : in     String)
   is
      Box : Gtk.Box.Gtk_Vbox;
   begin
      Default_Dialog.Initialize (Dialog, Title, Default_Dialog.Button_Cancel);

	  Box := Default_Dialog.Get_Center_Box (Dialog);

      Gtk.Label.Gtk_New (Dialog.Progress_Label, "Status");
	  Gtk.Box.Pack_Start (Box, Dialog.Progress_Label, Expand => False, 
						  Fill => False, Padding => DEFAULT_SPACING);

      Gtk.Progress_Bar.Gtk_New (Dialog.Progress_Bar);
	  Gtk.Box.Pack_Start (Box, Dialog.Progress_Bar, Expand => False, 
						  Fill => False, Padding => DEFAULT_SPACING);
   end;

end Giant.Progress_Dialog;
