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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-config-vis_styles-test.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/06/25 17:28:05 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Config;
with Giant.Config.Vis_Styles;
with Giant.Graph_Lib;

with Giant.Logger;

package body Giant.Config.Vis_Styles.Test is

   package Logger is new Giant.Logger("giant.config.vis_styles.test");

   procedure Test_Initialisation
     (R : in out AUnit.Test_Cases.Test_Case'Class) is

   begin

      for i in 1 .. 1 loop

        Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
          ("resources/vis_styles/resources_dir",
           "",
           "",
           "resources/vis_styles/only_defaults_giant_vis_style.xml");

    --    Assert (Giant.Config.Vis_Styles.Get_Number_Of_Known_Vis_Styles = 1,
      --          "Test whether ammount of loaded vis styles is correct");


        Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
      end loop;
   end Test_Initialisation;


   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Config.Vis_Styles.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Initialisation'Access, "Initialisation");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Create
      ("resources/"
       & "rfg_examp.iml");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Config.Vis_Styles.Test;
