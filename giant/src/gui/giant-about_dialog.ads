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
--  $RCSfile: giant-about_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/21 21:04:02 $
--
------------------------------------------------------------------------------
--
-- Provides the about dialog.
--

with Ada.Strings.Unbounded;

with Gtk.Text;
with Gtk.Window;

with Giant.Default_Dialog;

package Giant.About_Dialog is

   type About_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type About_Dialog_Access is access all About_Dialog_Record'Class;

   procedure Show;

private
   type About_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with null record;

end Giant.About_Dialog;
