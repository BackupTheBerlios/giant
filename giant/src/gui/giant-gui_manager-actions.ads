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
--  $RCSfile: giant-gui_manager-actions.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
--  Manages the actions that have global visiblity i.e. are pending
--  for all graph windows.
--

with Gdk.Event;

with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Window;
with Giant.Vis;

package Giant.Gui_Manager.Actions is

   procedure Set_Global_Action
     (Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   procedure Set_Local_Action
     (Window_Name : in     String;
      Action      : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   function Is_Action_Pending
     return Boolean;

   procedure Cancel;

   procedure Trigger
     (Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action);

private

   Pending_Action : Graph_Window.Actions.Graph_Window_Action_Access;

end Giant.Gui_Manager.Actions;

