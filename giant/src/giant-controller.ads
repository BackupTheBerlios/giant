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
--  $RCSfile: giant-controller.ads,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/06/18 15:16:26 $
--
------------------------------------------------------------------------------
--
--  Contains the controller. The controller is responsible for maintaing
--  data consistency. All operations that modify global data must be
--  included in this package.
--

with Giant.Projects;
with Giant.Valid_Names;

package Giant.Controller is

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Creates a new project.
   --
   --  See:
   --    Project_Management.Create_New_Empty_Project
   procedure Create_Project
     (Name              : in String;
      Project_Directory : in String;
      IML_Graph_File    : in String);

   ---------------------------------------------------------------------------
   --  Returns the currently open project.
   --
   function Get_Project
     return Projects.Project_Access;

   procedure Open_Project
     (Filename : in String);

   procedure Save_Project;

   procedure Save_Project
     (Filename : in String);

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

   procedure Remove_Window
     (Name : in String);

private

   Current_Project : Projects.Project_Access;

end Giant.Controller;
