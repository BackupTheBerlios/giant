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
--  $RCSfile: giant-main_window-actions.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/07/05 20:13:42 $
--

package body Giant.Main_Window.Actions is

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------

   function Create
     (Subgraph_Name : in String)
     return Create_Selection_Action_Access
   is
      Action : Create_Selection_Action_Access;
   begin
      Action := new Create_Selection_Action_Type (Subgraph_Name'Length);
      Action.Subgraph_Name := Subgraph_Name;
      return Action;
   end Create;

   procedure Cancel
     (Action : access Create_Selection_Action_Type)
   is
   begin
      Destroy (Action);
   end Cancel;

   procedure Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d)
   is
   begin
      Controller.Create_Selection_From_Subgraph
        (Action.Subgraph_Name,
         Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window)),
         Action.Subgraph_Name);
   end Execute;

end Giant.Main_Window.Actions;
