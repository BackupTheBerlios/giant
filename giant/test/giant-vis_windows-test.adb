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
--  $RCSfile: giant-vis_windows-test.adb,v $, $Revision: 1.5 $
--  $Author: schwiemn $
--  $Date: 2003/07/03 13:15:39 $
--
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with String_Lists;

with Giant.Logger;

with Giant.Config;
with Giant.Config.Vis_Styles;

with Giant.Vis_Windows;
with Giant.Graph_Lib.Selections;

package body Giant.Vis_Windows.Test is

   package Logger is new Giant.Logger("Giant.Vis_Windows.Test");


   ---------------------------------------------------------------------------
   function Set_Up_Vis_Window (Name : in String)
     return Visual_Window_Access is

      New_Vis_Window : Visual_Window_Access;
      Selection_1 : Graph_Lib.Selections.Selection;
      Selection_2 : Graph_Lib.Selections.Selection;
      Selection_3 : Graph_Lib.Selections.Selection;

   begin

      New_Vis_Window := Vis_Windows.Create_New (Name);

      Selection_1 := Graph_Lib.Selections.Create ("Selection_1");
      Selection_2 := Graph_Lib.Selections.Create ("Selection_2");
      Selection_3 := Graph_Lib.Selections.Create ("Selection_3");

      Vis_Windows.Add_Selection (New_Vis_Window, Selection_1);
      Vis_Windows.Add_Selection (New_Vis_Window, Selection_2);
      Vis_Windows.Add_Selection (New_Vis_Window, Selection_3);

      return New_Vis_Window;
   end Set_Up_Vis_Window;

   ---------------------------------------------------------------------------
   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Window : Vis_Windows.Visual_Window_Access;
      Test_List : String_Lists.List;
   begin

      Test_Window := Set_Up_Vis_Window ("Test_Window_X");

      -- check status;
      Assert (Get_Name (Test_Window) = "Test_Window_X",
              "Test_Vis_Window_Name");

      Test_List := Vis_Windows.Get_All_Selections (Test_Window);

      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of selection list");

      String_Lists.Destroy (Test_List);



   end Test_Init;

   ---------------------------------------------------------------------------
   procedure Test_Changing_Names
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

   begin

      null;
   end  Test_Changing_Names;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Vis_Windows.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Test_Init");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin

      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load
        ("resources/rfg_examp.iml");
      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("resources/vis_styles/resources_dir",
         "",
         "",
         "resources/vis_styles/only_defaults_giant_vis_style.xml");
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin

      Giant.Graph_Lib.Destroy;
      Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
   end Tear_Down;

end Giant.Vis_Windows.Test;
