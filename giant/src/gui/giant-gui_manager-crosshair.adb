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
--  $RCSfile: giant-gui_manager-crosshair.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/23 19:19:34 $

package body Giant.Gui_Manager.Crosshair is
   
   procedure Enqueue
     (Action : access Crosshair_Action_Type'Class)
   is
   begin
	  Set_Crosshair_Mode (True);
   end Enqueue;
   
   function Is_Action_Enqueued
	 return Boolean
   is
   begin
	  return (Pending_Action /= null);
   end Is_Action_Enqueued;

   procedure Trigger
	 (Window : access Graph_Window.Graph_Window_Record'Class)
   is
   begin
	  if (Pending_Action /= null) then
		 Execute (Pending_Action, Window);
	  end if;
	  Set_Crosshair_Mode (False);
   end Trigger;
   
end Giant.Gui_Manager.Crosshair;

