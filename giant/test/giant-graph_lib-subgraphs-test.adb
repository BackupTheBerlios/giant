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
--  $RCSfile: giant-graph_lib-subgraphs-test.adb,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/06/23 15:25:07 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Logger;

package body Giant.Graph_Lib.Subgraphs.Test is

   -------------------------------------
   --  Global variables and routines  --
   -------------------------------------

   package Logger is new Giant.Logger("giant.graph_lib.subgraphs.test");

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   --  Init of the graph
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Create ("resources/rfg_examp.iml");
   end Init;

   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Destroy;
   end Done;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Graph_Lib");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Init'Access, "Init");
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

end Giant.Graph_Lib.Subgraphs.Test;
