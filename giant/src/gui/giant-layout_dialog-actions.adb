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
--  $RCSfile: giant-layout_dialog-actions.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/09/01 22:09:11 $
--

with Giant.Controller;
with Giant.Logger;

package body Giant.Layout_Dialog.Actions is

   package Logger is new Giant.Logger("giant.layout_dialog.actions");

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------

   function Create
     (Dialog : Layout_Dialog_Access)
     return Set_Position_Action_Access
   is
      Action : Set_Position_Action_Access;
   begin
      Action := new Set_Position_Action_Type;
      Action.Dialog := Dialog;
      return Action;
   end Create;

   procedure Cancel
     (Action : access Set_Position_Action_Type)
   is
   begin
      Logger.Warn ("Destroying layout dialog.");
      Destroy (Action.Dialog);
   end Cancel;

   function Execute
     (Action   : access Set_Position_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean
   is
   begin
      Layout_Dialog.Apply_Layout (Action.Dialog, Event.Location);
      return True;
   end Execute;

end Giant.Layout_Dialog.Actions;
