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
--  $RCSfile: giant-projects-test.adb,v $, $Revision: 1.5 $
--  $Author: schwiemn $
--  $Date: 2003/06/25 15:46:45 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Logger;
with Giant.Projects;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Vis_Windows;

package body Giant.Projects.Test is

   package Logger is new Giant.Logger("Giant.Projects.Test");

   procedure Test_Initialize (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   
      New_Project : Giant.Projects.Project_Access;
      
      Subgraph_Donald : Giant.Graph_Lib.Subgraphs.Subgraph;
      Subgraph_Daisy  : Giant.Graph_Lib.Subgraphs.Subgraph;
      
      Vis_Window_Durchsicht : Giant.Vis_Windows.Visual_Window_Access;      
      
      
   begin
      
      New_Project := Giant.Projects.Create_Empty_Project 
        ("My_Test_Project",
         "resources/test_project_directory/",
         "resources/rfg_examp.iml",
         Giant.Graph_Lib.Get_Graph_Hash);
   
      Assert (Projects.Does_Project_Exist_File
        ("/resources/test_project_directory/My_Test_Project.xml"),
         "Does_Project_Exist");
                          
      Subgraph_Donald := Giant.Graph_Lib.Subgraphs.Create ("Donald");
      Subgraph_Daisy  := Giant.Graph_Lib.Subgraphs.Create ("Daisy");
         
      Vis_Window_Durchsicht := Giant.Vis_Windows.Create_New ("Durchsicht");
         
      Giant.Projects.Add_Subgraph (New_Project, Subgraph_Donald);
      Giant.Projects.Add_Subgraph (New_Project, Subgraph_Daisy);
      
      Giant.Projects.Add_Visualisation_Window 
        (New_Project, Vis_Window_Durchsicht);
      
      Projects.Store_Whole_Project_As
         (New_Project,
          "Copy_Of_My_Test_Project",
          "resources/test_project_copy_dir");
          
      Projects.Deallocate_Project_Deep (New_Project);
   
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Projects - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Initialize'Access, "Inititialisation");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Create 
        ("/home/schwiemn/giant/schwiemn/CVS_Hpro/giant/test/resources/"
         & "rfg_examp.iml");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Projects.Test;
