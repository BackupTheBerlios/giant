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
--  $RCSfile: giant-main_window-actions.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--


with Giant.Controller;

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
      null;
   end Cancel;

   function Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
     return Boolean
   is
   begin
      Controller.Create_Selection_From_Subgraph
        (Action.Subgraph_Name,
         Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window)),
         Action.Subgraph_Name);
      return True;
   end Execute;

end Giant.Main_Window.Actions;
