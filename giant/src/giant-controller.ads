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
--  $RCSfile: giant-controller.ads,v $, $Revision: 1.18 $
--  $Author: squig $
--  $Date: 2003/06/25 16:07:51 $
--
------------------------------------------------------------------------------
--
--  Contains the controller. The controller is responsible for maintaing
--  data consistency. All operations that modify global data must be
--  included in this package.
--

with Giant.Graph_Lib;
with Giant.Projects;
with Giant.Valid_Names;

package Giant.Controller is

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   procedure Execute_GSL
     (Script : in String);

   ---------------------------------------------------------------------------
   --  GUI
   ---------------------------------------------------------------------------

   procedure Show_Gui;

   function Hide_Gui
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   function Get_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
     return String;

   function Is_Node_Annotated
     (Node : in Graph_Lib.Node_Id)
     return Boolean;

   procedure Set_Node_Annotation
     (Node : in Graph_Lib.Node_Id;
      Text : in String);

   procedure Remove_Node_Annotation
     (Node : in Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Creates a new project.
   --
   --  See:
   --    Giant.Graph_Lib.Create
   --    Giant.Projects.Create_Empty_Project_For_File
   procedure Create_Project
     (Filename       : in String;
      Graph_Filename : in String);

   ---------------------------------------------------------------------------
   --  Returns the currently open project.
   --
   function Get_Project
     return Projects.Project_Access;

   function Get_Unique_Name
     (Name : in String := "Unknown")
      return String;

   function Is_Project_Loaded
     return Boolean;

   procedure Open_Project
     (Filename : in String);

   procedure Save_Project;

   procedure Save_Project
     (Filename : in String);


   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String := "Name");

   procedure Create_Selection_From_Subgraph
     (Subgraph_Name : in String;
      Window_Name   : in String;
      Name          : in String);

   procedure Duplicate_Selection
     (Window_Name : in String;
      Source_Name : in String;
      Target_Name : in String);

   procedure Hide_Selection
     (Window_Name    : in String;
      Selection_Name : in String);

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Show_All_Selections
     (Window_Name    : in String);

   procedure Show_Selection
     (Window_Name    : in String;
      Selection_Name : in String);

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
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Create_Window
     (Name : in String := "Unknown");

   procedure Open_Window
     (Name : in String);

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

private

   Current_Project : Projects.Project_Access;
   Project_Loaded : Boolean := False;

end Giant.Controller;
