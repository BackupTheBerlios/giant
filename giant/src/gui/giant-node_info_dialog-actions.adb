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
--  $RCSfile: giant-node_info_dialog-actions.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/08/25 16:06:25 $
--


with Giant.Controller;

package body Giant.Node_Info_Dialog.Actions is

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------

   function Create
     (Dialog : in Node_Info_Dialog_Access)
     return Pick_Node_Action_Access
   is
      Action : Pick_Node_Action_Access;
   begin
      Action := new Pick_Node_Action_Type;
      Action.Dialog := Dialog;
      return Action;
   end Create;

   procedure Cancel
     (Action : access Pick_Node_Action_Type)
   is
   begin
      Destroy (Action);
   end Cancel;

   function Execute
     (Action   : access Pick_Node_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean
   is
      use type Graph_Widgets.Handlers.Pressed_On_Type;
   begin
      if (Event.Pressed_On = Graph_Widgets.Handlers.On_Node) then
         Node_Info_Dialog.Set_Node (Action.Dialog, Event.Node);
         return True;
      end if;

      --  do not cancel action mode until user has selected a node
      return False;
   end Execute;

end Giant.Node_Info_Dialog.Actions;
