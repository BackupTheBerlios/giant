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
--  $RCSfile: giant-set_operation_dialog.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/18 15:16:26 $
--

with Glib;
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Window;

with Giant.Default_Dialog;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Set_Operation_Dialog is

   procedure Create
     (Dialog : out Set_Operation_Dialog_Access)
   is
   begin
      Dialog := new Set_Operation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog  : access Set_Operation_Dialog_Record'Class)
   is
      Box : Gtk.Box.Gtk_Vbox;
   begin
      Default_Dialog.Initialize (Dialog, -"Set Operation",
                                 Default_Dialog.Button_Okay_Cancel);

      Box := Default_Dialog.Get_Center_Box (Dialog);
   end;

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

end Giant.Set_Operation_Dialog;
