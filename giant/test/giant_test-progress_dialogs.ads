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
--  $RCSfile: giant_test-progress_dialogs.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/02 00:09:08 $
--
------------------------------------------------------------------------------


with Gtk.Dialog;
with Gtk.Progress_Bar;
with Gtk.Button;

package Giant_Test.Progress_Dialogs is

   procedure Make_New_Progress_Dialog
     (Progress_Dialog :    out Gtk.Dialog.Gtk_Dialog;
      Progress_Bar    :    out Gtk.Progress_Bar.Gtk_Progress_Bar;
      Cancel_Button   :    out Gtk.Button.Gtk_Button);

end Giant_Test.Progress_Dialogs;
