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
--  $RCSfile: giant-gui_manager-actions.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/27 14:34:55 $
--
--  Manages the actions that have global visiblity i.e. are pending
--  for all graph windows.
--

with Gdk.Event;

with Giant.Graph_Window;
with Giant.Vis;

package Giant.Gui_Manager.Actions is

   procedure Set_Global_Action
     (Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   function Is_Action_Pending
     return Boolean;

   procedure Cancel;

   procedure Trigger
     (Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event;
      Location : in     Vis.Logic.Vector_2d);

private

   Pending_Action : Graph_Window.Actions.Graph_Window_Action_Access;

end Giant.Gui_Manager.Actions;

