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
--  $RCSfile: giant-layout_dialog-actions.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--

with Giant.Controller;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

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
