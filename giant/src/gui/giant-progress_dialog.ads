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
--  $RCSfile: giant-progress_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/02 01:04:18 $
--
------------------------------------------------------------------------------
--
-- Provides a progress dialog;
--

with Gtk.Label;
with Gtk.Progress_Bar;
with Gtk.Window;

with Giant.Default_Dialog;

package Giant.Progress_Dialog is
   
   type Progress_Dialog_Record is 
	 new Default_Dialog.Default_Dialog_Record with private;
	 
   type Progress_Dialog_Access is access all Progress_Dialog_Record'Class;
   
   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
     return Boolean;

   procedure Create 
	 (Dialog : out Progress_Dialog_Access;
	  Title	 : in     String);
   
   procedure Initialize
     (Dialog : access Progress_Dialog_Record'Class;
	  Title	 : in     String);

--     function Get_Progress_Bar 
--  	 return Gtk.Progress_Bar.Gtk_Progress_Bar;
	
--     function Get_Progress_Label
--  	 return Gtk.Label.Gtk_Label;
   
--      function Get_Cancel_Button
--  	  return Gtk.Button.Gtk_Button;

private
   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
		Progress_Label : Gtk.Label.Gtk_Label;
     end record;

end Giant.Progress_Dialog;
