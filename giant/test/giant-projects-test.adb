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
--  $RCSfile: giant-projects-test.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/17 15:05:37 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Projects;
with Giant.Default_Logger;

package body Giant.Projects.Test is

   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (Projects.Does_Project_Exist ("Test", "."),
              "Does_Project_Exist");
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Projects");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Init");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Default_Logger.Init;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Default_Logger.Close;
   end Tear_Down;

end Giant.Projects.Test;
