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
--  $RCSfile: giant-valid_names-test.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/16 15:41:09 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Valid_Names;
with Giant.Default_Logger;

package body Giant.Valid_Names.Test is

   procedure Test_Is_Standard_Name (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (Valid_Names.Is_Standard_Name ("Standard_Name"), "Standard_Name");
      Assert (not Valid_Names.Is_Standard_Name ("Standard Name"),
              "Standard Name");
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Valid_Names");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Is_Standard_Name'Access, "Is_Standard_Name");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Default_Logger.Init;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Default_Logger.Close;
   end Tear_Down;

end Giant.Valid_Names.Test;
