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
--  $RCSfile: giant-config-global_data-test.adb,v $, $Revision: 1.9 $
--  $Author: schwiemn $
--  $Date: 2003/09/15 19:34:04 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.File_Management;
with Giant.Config_Settings;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Config.Global_Data.Test is

   package Logger is new Giant.Logger("Giant.Vis_Windows.Test");

   procedure Test_Icons (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
   
      Logger.Debug ("Teste - Path Expansion for Annotations Icon");
      Giant.Config_Settings.Initialize_Config_Settings
        ("./resources/config_glob_test/global_config.xml",
         "");
      Giant.Config.Global_Data.Initialize_Config_Data;
      
      ----------
      Assert 
        (Giant.Config_Settings.Does_Setting_Exist 
          ("Icon_For_Node_Annotations"),
         "Check whether Icon_For_Node_Annotations setting exists");
         
      Assert
        ((Giant.Config_Settings.Get_Setting_As_String
          ("Icon_For_Node_Annotations") = "./annotation_dir/document.xpm"),
          "Check correct Icon_For_Node_Annotations path in config file");
                    
      -----------
      -- Loactes Annotations Icon using path relative to config file pos
      
      Logger.Debug ("Teste auf File");
      Logger.Debug ("--------------");
      
      
      Logger.Debug ("Expanded_Path:   "
        & Giant.Config_Settings.Get_Setting_With_Path_Expanded
           ("Icon_For_Node_Annotations"));
           
      Logger.Debug ("Calculated_Path: "
        & File_Management.Get_Absolute_Path_To_File_From_Relative
             ("./resources/config_glob_test/",
              "./annotation_dir/document.xpm"));
      
      Logger.Debug ("--------------");
      Assert 
        ((Giant.Config_Settings.Get_Setting_With_Path_Expanded
           ("Icon_For_Node_Annotations")
          = File_Management.Get_Absolute_Path_To_File_From_Relative
             ("./",
              "resources/config_glob_test/annotation_dir/document.xpm")),
         "check annotations icon rekative to config file 1");          
--      Assert 
--        ((Giant.Config.Global_Data.Get_Node_Annotations_Icon
--          = File_Management.Get_Absolute_Path_To_File_From_Relative
--            ("./",
--             "./resources/config_glob_test/annotation_dir/document.xpm")),
--         "check annotations icon relative to config file 2"); 
                                    
--      Logger.Debug ("Annotations_Icon_Path_Global: "
--        & Giant.Config.Global_Data.Get_Node_Annotations_Icon);
        
      -----------
--      -- Loactes Annotations Icon using absolute_path_root setting
--      Assert 
--        ((Giant.Config_Settings.Get_Setting_With_Path_Expanded
--           ("Icon_relative_to_aps_path_root")
--          = File_Management.Get_Absolute_Path_To_File_From_Relative
--             ("./",
--              "./resources/config_glob_test/annotation_dir/document.xpm")),
--         "check annotations icon relative to absolute_path_root node");          
        
        
--      ---------------------  
--      -- Check for empty paths
--      Assert 
--        ((Giant.Config_Settings.Get_Setting_With_Path_Expanded
--           ("Icon_should_not_be_found")
--          = ""),
--         "check empty path for not existing icon"); 

   end Test_Icons;

   
   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Config.Global_Data.Test");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Icons'Access, "Test_Icons");
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
