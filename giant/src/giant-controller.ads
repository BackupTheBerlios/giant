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
--  $RCSfile: giant-controller.ads,v $, $Revision: 1.27 $
--  $Author: squig $
--  $Date: 2003/07/01 16:28:35 $
--
------------------------------------------------------------------------------
--
--  Contains the controller. The controller is responsible for maintaing
--  data consistency. All operations that modify global data i.e. data
--  that is stored in the project must be included in this package.
--
--  Think of the controller as a facade for the framework of the
--  application. Sadly the facade is incomplete and only contains
--  methods that modify global data the inspectors are missing and
--  need to be called directly.
--
--  The controller also stores the currently loaded project. The
--  controller can only handle a singe project.
--
--  Frequently used parameters:
--    Ask_For_Confirmation -
--      If True, the content was modified and the gui is visible, the
--      user is prompted for confirmation. Usually the dialog contains a
--      Yes, No and Cancel button. If the user selects cancel the called
--      function usually returns False.
--      Otherwise, the requested operation is executed and True is
--      returned.
--

with Giant.Graph_Lib;
with Giant.Gsl;
with Giant.Gsl.Interpreters;
with Giant.Projects;
with Giant.Valid_Names;
with Giant.Vis;
with Giant.Vis_Windows;

package Giant.Controller is

   ---------------------------------------------------------------------------
   --  GUI
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Shows the main window.
   --
   --  See:
   --    Giant.Gui_Manager.Show
   procedure Show_Gui;

   ---------------------------------------------------------------------------
   --  Closes all windows. The current project is not closed.
   --
   --  Returns:
   --    True, if gui was hidden; False, if the user cancelled.
   --  See:
   --    Giant.Gui_Manager.Hide
   function Hide_Gui
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   procedure Execute_GSL
     (Filename : in String);

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------

   procedure Apply_Layout
     (Layout_Name           : in String;
      Window_Name           : in String;
      Selection_Name        : in String;
      Additional_Parameters : in String);

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns the annotation for Node.
   function Get_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Returns True, if Node is annotated.
   function Is_Node_Annotated
     (Node : in Graph_Lib.Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Sets the annotation for Node to Text.
   procedure Set_Node_Annotation
     (Node : in Graph_Lib.Node_Id;
      Text : in String);

   ---------------------------------------------------------------------------
   --  Removes the annotation for Node.
   procedure Remove_Node_Annotation
     (Node : in Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --  Pins
   ---------------------------------------------------------------------------

   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String;
      Position    : in Vis.Logic.Vector_2d;
      Zoom_Level  : in Vis.Zoom_Level);

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Show_Pin
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Closes the currently open project.
   --
   --  This method needs to be called prior to Create_Project and
   --  Open_Project if Is_Project_Loaded returns True.
   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Creates a new project.
   --
   --  See:
   --    Close_Project
   --    Giant.Graph_Lib.Load
   --    Giant.Projects.Create_Empty_Project_For_File
   procedure Create_Project
     (Filename       : in String;
      Graph_Filename : in String);

   ---------------------------------------------------------------------------
   --  Returns the currently open project.
   function Get_Project
     return Projects.Project_Access;

   ---------------------------------------------------------------------------
   --  Returns a unique name for the current project by appending
   --  digits to Name. This is useful for the creation of project
   --  global object like Vis_Windows and Subgraphs.
   function Get_Unique_Name
     (Name : in String := "Unknown")
      return String;

   ---------------------------------------------------------------------------
   --  Returns True, if the current project was modified. The return
   --  value is undefined if no project is loaded.
   function Has_Project_Changed
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns True, if a project is loaded; False, otherwise.
   function Is_Project_Loaded
     return Boolean;

   ---------------------------------------------------------------------------
   --  Opens a project.
   --
   --  See:
   --    Close_Project
   --    Giant.Projects.Get_Bauhaus_IML_Graph_Data_File
   --    Giant.Graph_Lib.Load
   --    Giant.Projects.Load_Project_File
   procedure Open_Project
     (Filename : in String);

   ---------------------------------------------------------------------------
   --  Saves the current project.
   --
   --  See:
   --    Giant.Projects.Store_Whole_Project
   procedure Save_Project;

   ---------------------------------------------------------------------------
   --  Saves the current project with in a different directory.
   --
   --  See:
   --    Giant.Projects.Store_Whole_Project_As_For_File
   procedure Save_Project
     (Filename : in String);

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Create_Selection_From_Subgraph
     (Subgraph_Name : in String;
      Window_Name   : in String;
      Name          : in String);

   procedure Duplicate_Selection
     (Window_Name : in String;
      Source_Name : in String;
      Target_Name : in String);

   procedure Hide_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Highlight_Selection
     (Window_Name      : in String;
      Name             : in String;
      Highlight_Status : in Vis_Windows.Selection_Highlight_Status);

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Set_Current_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Show_All_Selections
     (Window_Name    : in String);

   procedure Show_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Unhighlight_Selection
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Create_Subgraph
     (Name : in String := "Unknown");

   procedure Duplicate_Subgraph
     (Source_Name : in String;
      Target_Name : in String);

   function Remove_Subgraph
     (Name : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String);

   procedure Set_Subgraph_Highlight_Status
     (Name   : in String;
      Status : in Projects.Subgraph_Highlight_Status);

   procedure Subgraph_Difference
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Subgraph_Intersection
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Subgraph_Union
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   procedure Set_Vis_Style
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Creates a window with a unique name. The window is opened if the
   --  gui is shown.
   --
   --  See:
   --    Get_Unique_Name
   --    Giant.Vis_Windows.Create_New
   --    Giant.Projects.Add_Visualisation_Window
   --    Open_Window
   procedure Create_Window
     (Name : in String := "Unknown");

   ---------------------------------------------------------------------------
   --  Opens a window, if gui is shown.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Open
   procedure Open_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Removes a window.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Remove_Window
   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Renames a window.
   --
   --  Parameters:
   --    Old_Name - The name of the window
   --    New_Name - The new name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Projects.Change_Vis_Window_Name
   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

   ---------------------------------------------------------------------------
   --  Save a window.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Open
   procedure Save_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Set_Zoom_Level
     (Window_Name : in String;
      Zoom_Level  : in Vis.Zoom_Level);

private

   Current_Project : Projects.Project_Access;
   Project_Loaded : Boolean := False;
   Project_Changed : Boolean := False;

   Gsl_Interpreter : Gsl.Interpreters.Interpreter := null;

end Giant.Controller;
