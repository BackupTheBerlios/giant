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
--  $RCSfile: giant-node_info_dialog-actions.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/08/13 11:58:38 $
--
--  
--

with Gdk.Event;

with Giant.Graph_Window;
with Giant.Vis;

package Giant.Node_Info_Dialog.Actions is

   type Pick_Node_Action_Type is
     new Graph_Window.Actions.Graph_Window_Action_Type with record
        Dialog : Node_Info_Dialog_Access;
     end record;

   type Pick_Node_Action_Access is
     access all Pick_Node_Action_Type'Class;
   
   function Create 
	 (Dialog : in Node_Info_Dialog_Access)
	 return Pick_Node_Action_Access;

   procedure Cancel
     (Action : access Pick_Node_Action_Type);

   function Execute
     (Action   : access Pick_Node_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d)
	 return Boolean;
      
End Giant.Node_Info_Dialog.Actions;
