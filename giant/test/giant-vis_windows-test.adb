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
--  $RCSfile: giant-vis_windows-test.adb,v $, $Revision: 1.6 $
--  $Author: schwiemn $
--  $Date: 2003/07/08 14:50:40 $
--
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Gtk.Main;


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
      
      Vis_Windows.Add_Pin 
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_1",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.1);
         
      Vis_Windows.Add_Pin 
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_2",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.2);
         
      Vis_Windows.Add_Pin 
        (Vis_Window => New_Vis_Window,
         Name       => "Pin_3",
         Position   => Vis.Logic.Zero_2d,
         Zoom_Level => 0.3);

      return New_Vis_Window;
   end Set_Up_Vis_Window;
   
   ---------------------------------------------------------------------------
   procedure Check_Default_Win_Status 
     (Test_Window : in Vis_Windows.Visual_Window_Access) is

      Test_List      : String_Lists.List;      
      Test_Selection : Graph_Lib.Selections.Selection;
   begin

      -- check status;
      Assert (Get_Name (Test_Window) = "Test_Window_X",
              "Test_Vis_Window_Name");
      
      -- check selections
      --------------------

      -- all selections
      Test_List := Vis_Windows.Get_All_Selections (Test_Window);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of selection list");
      String_Lists.Destroy (Test_List);
      
      Assert 
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_1"),
         "Test whether ""Selection_2"" exists");         
      Assert 
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_2"),
         "Test whether ""Selection_3"" exists");         
      Assert 
        (Vis_Windows.Does_Selection_Exist (Test_Window, "Selection_3"),
         "Test whether ""Selection_3"" exists");
         
      Test_Selection := 
        Vis_Windows.Get_Selection (Test_Window, "Selection_1");      
      Assert 
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_1",
         "Test Get_Selection retrieving ""Selection_1");
            
      Test_Selection := 
        Vis_Windows.Get_Selection (Test_Window, "Selection_2");      
      Assert 
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_2",
         "Test Get_Selection retrieving ""Selection_2");
            
      Test_Selection := 
        Vis_Windows.Get_Selection (Test_Window, "Selection_3");      
      Assert 
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Selection_3",
         "Test Get_Selection retrieving ""Selection_3");
           
      Test_Selection := 
        Vis_Windows.Get_Selection (Test_Window, "Default");      
      Assert 
        (Graph_Lib.Selections.Get_Name (Test_Selection) = "Default",
         "Test Get_Selection retrieving ""Default");         
                
      -- standard selection
      Assert (Vis_Windows.Get_Standard_Selection (Test_Window) = "Default",
              "Test_Name_of_Standard_Selection");
              
      -- check current selection
      Assert (Vis_Windows.Get_Current_Selection (Test_Window) = "Default",
              "Test_Name_of_Current_Selection");
                 
      -- test highlight status
      Assert (Get_Highlight_Status 
        (Test_Window,  
         Vis_Windows.Get_Current_Selection (Test_Window)) 
         = Giant.Vis_Windows.Current_Selection, 
         "Test_Current_Selection_Highlight_Status");
         
      -- test pins
      ------------
      Test_List := Get_All_Pins (Test_Window);
      Assert (String_Lists.Length (Test_List) = 3,
              "Test Length of pin list");  
      String_Lists.Destroy (Test_List);
              
      Assert 
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_1"),
         "Test whether ""Pin_1"" exists");         
      Assert 
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_2"),
         "Test whether ""Pin_2"" exists");         
      Assert 
        (Vis_Windows.Does_Pin_Exist (Test_Window, "Pin_3"),
         "Test whether ""Pin_3"" exists");
         

                                               
   end Check_Default_Win_Status;

   ---------------------------------------------------------------------------
   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Window : Vis_Windows.Visual_Window_Access;
      

   begin

      Test_Window := Set_Up_Vis_Window ("Test_Window_X");

      Check_Default_Win_Status (Test_Window);
      
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

   ---------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Test_Init");
      
      null;
   end Register_Tests;

   ---------------------------------------------------------------------------
   procedure Set_Up (T : in out Test_Case) is
   begin
   
      Gtk.Main.Init;

      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load
        ("resources/rfg_examp.iml");
      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("resources/vis_styles/resources_dir",
         "",
         "",
         "resources/vis_styles/only_defaults_giant_vis_style.xml");
   end Set_Up;

   ---------------------------------------------------------------------------
   procedure Tear_Down (T : in out Test_Case) is
   begin

      Giant.Graph_Lib.Destroy;
      Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
      
   --   Gtk.Main.Gtk_Exit (0);
   end Tear_Down;

end Giant.Vis_Windows.Test;
