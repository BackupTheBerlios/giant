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
--  $RCSfile: giant-gsl_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/05/31 19:23:40 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Gtk.Box;
with Gtk.Hbutton_Box;
with Gtk.Text;
with Gtk.Window;

with Giant.Default_Dialog;

package Giant.Gsl_Dialog is

   type Gsl_Dialog_Record is 
	 new Default_Dialog.Default_Dialog_Record with private;

   type Gsl_Dialog_Access is access all Gsl_Dialog_Record'Class;

   function Can_Hide
     (Dialog : access Gsl_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog : out Gsl_Dialog_Access);

   procedure Initialize
     (Dialog : access Gsl_Dialog_Record'class);

private
   type Gsl_Dialog_Record is
	 new Default_Dialog.Default_Dialog_Record with record  
		Filename : Ada.Strings.Unbounded.Unbounded_String;
		Text_Area : Gtk.Text.Gtk_Text;
	 end record;

end Giant.Gsl_Dialog;
