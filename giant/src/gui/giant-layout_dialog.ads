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
--  $RCSfile: giant-layout_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/05 20:13:42 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Giant.Default_Dialog;
with Giant.Vis;

package Giant.Layout_Dialog is

   type Layout_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Layout_Dialog_Access is
      access all Layout_Dialog_Record'Class;

   function Can_Hide
     (Dialog : access Layout_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog		 :    out Layout_Dialog_Access;
	  Window_Name	 : in     String;
	  Selection_Name : in     String);

   procedure Initialize
     (Dialog		 : access Layout_Dialog_Record'Class;
	  Window_Name	 : in     String;
	  Selection_Name : in     String);

   procedure Show
	 (Window_Name	 : in String;
	  Selection_Name : in String;
	  Position		 : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d);

private
   type Layout_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
		Layouts_Notebook : Gtk.Notebook;
		Position : Vis.Logic.Vector_2d := Vis.Logic.Zero_2d;
		Selection_Name : Ada.Strings.Unbounded.Unbounded_String;
		Window_Name : Ada.Strings.Unbounded.Unbounded_String;
     end record;

end Giant.Layout_Dialog;
