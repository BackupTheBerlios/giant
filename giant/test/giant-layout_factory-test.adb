------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und ‹bersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-layout_factory-test.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

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
