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
--  $RCSfile: giant-gui_manager-crosshair.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/23 21:57:04 $
--
--  Provides the cross window actions.
--

--  with Giant.Graph_Widgets;
--  with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Window;

package Giant.Gui_Manager.Crosshair is

--     type Crosshair_Action_Callback is access function
--       (Widget   : access Graph_Widgets.Graph_Widget_Record'Class;
--        Action   : in     Graph_Widgets.Notifications.Crosshair_Action_Type;
--        Location : in     Vis.Logic.Vector_2d)
--       return Boolean;
   type Crosshair_Action_Type is abstract tagged private;

   type Crosshair_Action_Access is access all Crosshair_Action_Type'Class;

   procedure Cancel
     (Action : access Crosshair_Action_Type)
      is abstract;

   procedure Destroy
     (Action : access Crosshair_Action_Type);

   procedure Execute
     (Action : access Crosshair_Action_Type;
      Window : access Graph_Window.Graph_Window_Record'Class)
      is abstract;

   procedure Enqueue
     (Action : access Crosshair_Action_Type'Class);

   function Is_Action_Enqueued
     return Boolean;

   procedure Trigger
     (Window : access Graph_Window.Graph_Window_Record'Class);

private

   type Crosshair_Action_Type is abstract tagged null record;

   Pending_Action : Crosshair_Action_Access;

end Giant.Gui_Manager.Crosshair;

