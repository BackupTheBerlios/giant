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
--  $RCSfile: giant-projects-test.adb,v $, $Revision: 1.4 $
--  $Author: schwiemn $
--  $Date: 2003/06/24 20:21:05 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Logger;
with Giant.Projects;
with Giant.Graph_Lib;

package body Giant.Projects.Test is

   package Logger is new Giant.Logger("Giant.Projects.Test");

   procedure Test_Initialize (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   
     New_Project : Giant.Projects.Project_Access;
   begin
      
      New_Project := Giant.Projects.Create_Empty_Project 
        ("My_Test_Project",
         "/home/schwiemn/giant/schwiemn/CVS_Hpro/giant/test/resources/"
         & "test_project_directory",
         "/home/schwiemn/giant/schwiemn/CVS_Hpro/giant/test/resources/"
         & "rfg_examp.iml",
         Giant.Graph_Lib.Get_Graph_Hash);
   
     --  Assert (Projects.Does_Project_Exist ("Test", "."),
     --         "Does_Project_Exist");
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
