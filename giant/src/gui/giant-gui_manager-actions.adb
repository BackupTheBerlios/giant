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
--  $RCSfile: giant-gui_manager-actions.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/09/08 15:33:10 $

with Giant.Graph_Window;

use type Giant.Graph_Window.Actions.Graph_Window_Action_Access;

package body Giant.Gui_Manager.Actions is

   procedure Set_Global_Action
     (Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class)
   is
   begin
      --  cancel currently pending action
      Cancel;

      Gui_Manager.Set_Status (-"Please select the target window");
      Pending_Action
        := Graph_Window.Actions.Graph_Window_Action_Access (Action);

      --  start action mode for all open windows
      Set_Action_Mode (True);
   end Set_Global_Action;

   procedure Set_Local_Action
     (Window_Name : in     String;
      Action      : access Graph_Window.Actions.Graph_Window_Action_Type'Class)
   is
      use type Graph_Window.Graph_Window_Access;
      Window : Graph_Window.Graph_Window_Access
        := Get_Open_Window (Window_Name);
   begin
      if (Window /= null) then
         Graph_Window.Set_Local_Action (Window, Action);
      end if;
   end Set_Local_Action;

   function Is_Action_Pending
     return Boolean
   is
   begin
      return (Pending_Action /= null);
   end Is_Action_Pending;

   procedure Cancel
   is
   begin
      if (Pending_Action /= null) then
         Gui_Manager.Set_Status ("");
         Graph_Window.Actions.Cancel (Pending_Action);
         Graph_Window.Actions.Destroy (Pending_Action);
         Pending_Action := null;
         Set_Action_Mode (False);
      end if;
   end Cancel;

   procedure Trigger
     (Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
   begin
      if (Pending_Action /= null) then
         Gui_Manager.Set_Status ("");
         if (Graph_Window.Actions.Execute (Pending_Action, Window, Event)) then
            Graph_Window.Actions.Destroy (Pending_Action);
            Pending_Action := null;
            Set_Action_Mode (False);
         end if;
      end if;
   end Trigger;

end Giant.Gui_Manager.Actions;

