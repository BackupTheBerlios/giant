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
--  $RCSfile: giant-config-vis_styles-test.adb,v $, $Revision: 1.8 $
--  $Author: schwiemn $
--  $Date: 2003/07/10 14:20:20 $
--
with Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.File_Management;
with Giant.Config;
with Giant.Config.Vis_Styles;
with Giant.Graph_Lib;

with Giant.Logger;

package body Giant.Config.Vis_Styles.Test is

   package Logger is new Giant.Logger("giant.config.vis_styles.test");
   
   ---------------------------------------------------------------------------
   procedure Test_Leacks
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
     Def_Vis_Style : Giant.Config.Vis_Styles.Visualisation_Style_Access;
     
   begin

      for i in 1 .. 1 loop

        Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
          ("resources/vis_styles/resources_dir",
           "",
           "",
           "resources/vis_styles/only_defaults_giant_vis_style.xml");
      
         Assert (Giant.Config.Vis_Styles.Get_Number_Of_Known_Vis_Styles = 1,
           "Test whether ammount of loaded vis styles is correct");
            
         Def_Vis_Style := Giant.Config.Vis_Styles.Get_Default_Vis_Style;
                         
         Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;

      end loop;
   end Test_Leacks;
   
   ---------------------------------------------------------------------------
   -- Only tests dafault vis style
   procedure Test_Init_Test_Set_Default_1
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
     Test_Vis_Style : Giant.Config.Vis_Styles.Visualisation_Style_Access;
     All_Icons : Giant.Config.Vis_Styles.Node_Icons_Array_Access;
     Node_Icon_Id : Positive;
   begin

      for i in 1 .. 1 loop

        Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
          ("",
           "resources/vis_styles/vis_styles_test_set_1/",
           "resources/vis_styles/vis_styles_test_set_2/",
           "resources/vis_styles/vis_styles_test_set_default_1/"
           & "test_vis_style_1_default.xml");
      
         Assert (Giant.Config.Vis_Styles.Get_Number_Of_Known_Vis_Styles = 3,
           "Test whether ammount of loaded vis styles is correct");
            
         Test_Vis_Style := 
           Giant.Config.Vis_Styles.Initialize_Vis_Style_By_Name
             ("test_vis_style_1_default");
         
         Assert (Config.Vis_Styles.Get_Default_Vis_Style = Test_Vis_Style,
           "Check whether Default Vis Style Loaded");
             
         All_Icons := Giant.Config.Vis_Styles.Get_All_Node_Icons;
         
         -- Check "test_vis_style_1_default"
         -----------------------------------
         
         -- Check Node Class Data
         ------------------------   
                
         -- icon test
         Node_Icon_Id := Giant.Config.Vis_Styles.Get_Node_Icon_Encoding
             (Test_Vis_Style, 
              Graph_Lib.Convert_Node_Class_Name_To_Id ("TC_Floating_Point"));              
         Assert 
           (Ada.Strings.Unbounded.To_String 
             (All_Icons.all (Node_Icon_Id)) 
            = File_Management.Get_Absolute_Path_To_File_From_Relative
               ("./", 
                "resources/vis_styles/vis_styles_test_set_default_1/"
                & "test_node_icon_default_1.xpm"), 
                "Teste_Icon ""TC_Floating_Point""" );
                
         -- icon test
         Node_Icon_Id := Giant.Config.Vis_Styles.Get_Node_Icon_Encoding
             (Test_Vis_Style, 
              Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"));              
         Assert 
           (Ada.Strings.Unbounded.To_String 
             (All_Icons.all (Node_Icon_Id)) 
            = File_Management.Get_Absolute_Path_To_File_From_Relative
               ("./", 
                "resources/vis_styles/vis_styles_test_set_default_1/"
                & "test_node_icon_blue_1.xpm"), 
                "Teste_Icon ""HPGNode""" );     
                
         -- icon test
         Node_Icon_Id := Giant.Config.Vis_Styles.Get_Node_Icon_Encoding
             (Test_Vis_Style, 
              Graph_Lib.Convert_Node_Class_Name_To_Id ("TC_Boolean"));              
         Assert 
           (Ada.Strings.Unbounded.To_String 
             (All_Icons.all (Node_Icon_Id)) 
            = File_Management.Get_Absolute_Path_To_File_From_Relative
               ("./", 
                "resources/vis_styles/vis_styles_test_set_default_1/"
                & "test_node_icon_red_1.xpm"), 
                "Teste_Icon ""TC_Boolean""" );   
      
                         
         Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
      end loop;
   end Test_Init_Test_Set_Default_1;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Config.Vis_Styles.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Leacks'Access, "Test_Leacks");
      Register_Routine 
        (T, Test_Init_Test_Set_Default_1'Access, 
         "Test_Init_Test_Set_Default_1");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Initialize;      
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Config.Vis_Styles.Test;
