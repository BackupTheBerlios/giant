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
--  $RCSfile: giant-selection_operation_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/18 15:41:06 $
--
------------------------------------------------------------------------------
--
--
--

with Ada.Strings.Unbounded;

with Giant.Set_Operation_Dialog;

package Giant.Selection_Operation_Dialog is

   type Selection_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with private;

   type Selection_Operation_Dialog_Access is
     access all Selection_Operation_Dialog_Record'Class;

   procedure Create
     (Dialog	  :    out Selection_Operation_Dialog_Access;
	  Window_Name : in     String);

   procedure Initialize
     (Dialog	  : access Selection_Operation_Dialog_Record'Class;
	  Window_Name : in     String);
   
   function Can_Hide
     (Dialog : access Selection_Operation_Dialog_Record)
     return Boolean;

private
   
   type Selection_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with record
		Window_Name : Ada.Strings.Unbounded.Unbounded_String;
	 end record;
   

end Giant.Selection_Operation_Dialog;
