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
--  $RCSfile: giant-layout_dialog-actions.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/08/25 16:06:25 $
--
--  Provides the main window. The main window is only instanciated once.
--

with Gdk.Event;

with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Window;
with Giant.Vis;

package Giant.Layout_Dialog.Actions is

   type Set_Position_Action_Type is
     new Graph_Window.Actions.Graph_Window_Action_Type with record
        Dialog : Layout_Dialog_Access;
     end record;

   type Set_Position_Action_Access is
     access all Set_Position_Action_Type'Class;

   function Create
     (Dialog : Layout_Dialog_Access)
     return Set_Position_Action_Access;

   procedure Cancel
     (Action : access Set_Position_Action_Type);

   function Execute
     (Action   : access Set_Position_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean;

end Giant.Layout_Dialog.Actions;
