------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-gui_manager-actions.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $

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

