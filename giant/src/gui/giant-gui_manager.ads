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
--  $RCSfile: giant-gui_manager.ads,v $, $Revision: 1.27 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
--  Stores the window records. Handles the controller updates. Provides
--  a facade for the gui.
--
--  Pattern:
--    ADT
--

with Giant.Graph_Lib;
with Giant.Graph_Window;
with Giant.Progress_Dialog;
with Giant.Vis_Windows;

package Giant.Gui_Manager is

   type For_Each_Graph_Window_Type is access procedure
     (Window : in Graph_Window.Graph_Window_Access);

   ---------------------------------------------------------------------------
   --  Main Application
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Shows the main window.
   procedure Show;

   ---------------------------------------------------------------------------
   --  Hides the main window.
   --
   --  Parameters:
   --    Ask_For_Confirmation - If True and project is modified, ask
   --    the user to save the project
   --  Returns:
   --    True, if the windows were closed; False, if the user
   --    cancelled.
   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   function Create_Progress_Dialog
     (Title   : in String;
      Message : in String)
     return Progress_Dialog.Progress_Dialog_Access;

   procedure Initialize_Project;

   function Is_Initialized
     return Boolean;

   procedure Set_Status
     (Text : in String);

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   procedure Update_Node_Annotation
     (Node : in Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --  Pins
   ---------------------------------------------------------------------------

   procedure Add_Pin
     (Window_Name : in String;
      Name        : in String);

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Update_Pin
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window_Name : in String;
      Name        : in String);

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Update_Selection
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Add_Subgraph
     (Name : in String);

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String);

   procedure Update_Subgraph
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   procedure Update_Vis_Style
     (Window_Name : in String);

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Update_Zoom_Level
     (Window_Name : in String);

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String);

   function Close
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Invokes Call_Procedure for each open graph window.
   generic
      with procedure Execute
        (Window : in Graph_Window.Graph_Window_Access);
   procedure For_Each_Open_Window;

   ---------------------------------------------------------------------------
   --  Returns the window with Name, if window is open; null, if
   --  window is not open.
   function Get_Open_Window (Name : in String)
     return Graph_Window.Graph_Window_Access;

   function Is_Window_Open
     (Name : in String)
     return Boolean;

   procedure Open
     (Visual_Window : Vis_Windows.Visual_Window_Access);

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

   ---------------------------------------------------------------------------
   --  Enables or disables the global action mode for all graph windows.
   procedure Set_Action_Mode
     (Activate : in Boolean);

   procedure Update_Window
     (Name : in String);

end Giant.Gui_Manager;

