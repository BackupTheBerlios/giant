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
--  $RCSfile: giant-main_window-actions.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/08/25 16:06:25 $
--
--  Provides the main window. The main window is only instanciated once.
--

with Gdk.Event;

with Giant.Graph_Window;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Vis;

package Giant.Main_Window.Actions is

   type Create_Selection_Action_Type (Name_Length : Positive) is
     new Graph_Window.Actions.Graph_Window_Action_Type with record
        Subgraph_Name : String(1 .. Name_Length);
     end record;

   type Create_Selection_Action_Access is
     access all Create_Selection_Action_Type'Class;

   function Create
     (Subgraph_Name : in String)
     return Create_Selection_Action_Access;

   procedure Cancel
     (Action : access Create_Selection_Action_Type);

   function Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean;

end Giant.Main_Window.Actions;
