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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/14 16:40:23 $
--

with Giant.Gui_Manager;
with Giant.Vis_Windows;

package body Giant.Controller is

--     ---------------------------------------------------------------------------
--     --  Creates a new project.
--     --
--     --  See:
--     --    Project_Management.Create_New_Empty_Project
--     function Create_Project
--       (Name							  : in String;
--  	  Project_Directory				  : in String;
--  	  Bauhaus_IML_Graph_File		  : in String;
--  	  Bauhaus_IML_Graph_File_Checksum : in Integer)
--  	 return Project_Access
--     is
--     begin
--  	  Current_Project := Project_Management.Create_New_Empty_Project
--  		(Valid_Names.To_Standard_Name (Name),
--  		 Project_Directory,
--  		 Bauhaus_IML_Graph_File,
--  		 Bauhaus_IML_Graph_File_Checksum);
--  	  return Current_Project;
--     end New_Project;

--     function Get_Project
--  	 return Project_Access
--     is
--     begin
--       return Current_Project;
--     end;

   procedure Create_Window
     (Name : in Valid_Names.Standard_Name)
   is 
	  Window : Vis_Windows.Visual_Window_Access;
   begin
	  Window := Vis_Windows.Create_New (Name);
	  Gui_Manager.Add (Window);
   end Create_Window;

   procedure Show 
   is
   begin 
	  Gui_Manager.Show;
   end Show;

end Giant.Controller;

