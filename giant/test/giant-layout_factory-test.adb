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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-layout_factory-test.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/07/14 14:13:53 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Logger;

package body Giant.Layout_Factory.Test is

   --------------------------------------------------------------------------
   package Logger is new Giant.Logger("layout_factory.test");

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Logger.Warn ("=== Running Layout Factory ===");
   end Test_Init;

   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      null;
   end Done;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Layout-Factory");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Init");
      Register_Routine (T, Done'Access, "Done");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.Layout_Factory.Test;
