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
--  $RCSfile: giant-config-global_data-test.adb,v $, $Revision: 1.2 $
--  $Author: schwiemn $
--  $Date: 2003/09/15 17:27:25 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Config_Settings;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Config.Global_Data.Test is

   package Logger is new Giant.Logger("Giant.Vis_Windows.Test");

   procedure Test_Icons (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
   
      Logger.Debug ("Test - AAAA");
      Giant.Config_Settings.Initialize_Config_Settings
        ("./resources/config_glob_test/global_config.xml",
         "");
      Assert 
        (Giant.Config_Settings.Does_Setting_Exist 
          ("Icon_For_Node_Annotations"),
         "Check whether Icon_For_Node_Annotations setting exists");

      --Config.Initialize_Config_Data ("resources/global_config.xml", "");
--      Assert (Config_Settings.Does_Setting_Exist ("Test_Setting"), "Test_Setting");
   end Test_Icons;

   
   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Config.Global_Data.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Icons'Access, "Init");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.Config.Global_Data.Test;
