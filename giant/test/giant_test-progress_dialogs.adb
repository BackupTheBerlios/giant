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
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-progress_dialogs.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/02 00:09:08 $
--
------------------------------------------------------------------------------


with Glib;

with Gdk.Main;
with Gdk.Threads;

with Gtk.Box;
with Gtk.Button;
with Gtk.Dialog;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Progress_Bar;
with Gtk.Widget;

package body Giant_Test.Progress_Dialogs is

   --  -----------------------------------------------------------------------
   --  -- Gui management
   --  -----------------------------------------------------------------------

   package Dialog_R_Boolean_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Dialog.Gtk_Dialog_Record, Boolean);

   function On_Progress_Dialog_Delete_Event
     (Dialog : access Gtk.Dialog.Gtk_Dialog_Record'Class)
      return Boolean is
   begin
      return True;
   end On_Progress_Dialog_Delete_Event;

   procedure Make_New_Progress_Dialog
     (Progress_Dialog :    out Gtk.Dialog.Gtk_Dialog;
      Progress_Bar    :    out Gtk.Progress_Bar.Gtk_Progress_Bar;
      Cancel_Button   :    out Gtk.Button.Gtk_Button) is
   begin
      Gtk.Button.Gtk_New (Cancel_Button, "Cancel");
      Gtk.Progress_Bar.Gtk_New (Progress_Bar);

      Gtk.Dialog.Gtk_New (Progress_Dialog);

      Gtk.Box.Add
        (Container => Gtk.Dialog.Get_Vbox (Progress_Dialog),
         Widget    => Progress_Bar);
      Gtk.Box.Add
        (Container => Gtk.Dialog.Get_Action_Area (Progress_Dialog),
         Widget    => Cancel_Button);

      Dialog_R_Boolean_Cb.Connect
        (Widget => Progress_Dialog,
         Name   => "delete_event",
         Marsh  => Dialog_R_Boolean_Cb.To_Marshaller
                     (On_Progress_Dialog_Delete_Event'Access));
   end Make_New_Progress_Dialog;

end Giant_Test.Progress_Dialogs;
