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
--  $RCSfile: giant-config-vis_styles-test.adb,v $, $Revision: 1.10 $
--  $Author: schwiemn $
--  $Date: 2003/07/10 18:33:15 $
--
with Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.File_Management;
with Giant.Config;
with Giant.Config.Vis_Styles;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Node_Attribute_Filters;

with Giant.Logger;

package body Giant.Config.Vis_Styles.Test is

   package Logger is new Giant.Logger("giant.config.vis_styles.test");
   
   ---------------------------------------------------------------------------
   -- Utilities
   ---------------------------------------------------------------------------
   
   ---------------------------------------------------------------------------
   -- Retruns True if all settings do match
   function Check_Edge_Class_Config_Status
     (Vis_Style      : in Config.Vis_Styles.Visualisation_Style_Access;
      Node_Class     : in String; 
      Attribute      : in String;
      Req_Line_Color : in String;
      Req_Text_Color : in String;
      Req_Line_Style : in Config.Vis_Styles.Edge_Line_Style;
      Show_Label     : in Boolean)                
   return Boolean is
   
      Passed       : Boolean := True;     
      All_Colors   : Giant.Config.Vis_Styles.Color_Access_Array_Access;
      Color_Id     : Positive;     
      A_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;      
   begin
   
      All_Colors := Giant.Config.Vis_Styles.Get_All_Colors;       
   
      A_Edge_Class := 
        GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
          (Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class),
           Graph_Lib.Convert_Node_Attribute_Name_To_Id              
            (Node_Class, Attribute));
                
      Color_Id := Config.Vis_Styles.Get_Line_Color 
        (Vis_Style, A_Edge_Class);
        
      if not (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
        Req_Line_Color) then
  
         Passed := False;
      end if;
           
      Color_Id := Config.Vis_Styles.Get_Text_Color 
        (Vis_Style, A_Edge_Class);
        
      if not (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
         Req_Text_Color) then
         
          Passed := False;         
      end if;
          
      if not (Giant.Config.Vis_Styles.Get_Line_Style 
        (Vis_Style, A_Edge_Class) = Req_Line_Style) then
        
         Passed := False;  
      end if;
                      
      if not (Giant.Config.Vis_Styles.Show_Label_For_Edge_Class_Name 
        (Vis_Style, A_Edge_Class) = Show_Label) then
        
         Passed := False;  
      end if;
         
      return Passed;
   end Check_Edge_Class_Config_Status;
            
   ---------------------------------------------------------------------------
   -- Retruns True if all settings do match
   function Check_Node_Class_Config_Status
     (Vis_Style          : in Config.Vis_Styles.Visualisation_Style_Access;
      Node_Class         : in String; 
      Req_Icon_File_Path : in String;      
      Req_Text_Color     : in String;
      Req_Border_Color   : in String;
      Req_Fill_Color     : in String;      
      Attr_in_Filter     : in Natural)                
   return Boolean is
   
      Passed       : Boolean := True;     
      All_Icons : Giant.Config.Vis_Styles.Node_Icons_Array_Access;  
      All_Colors : Giant.Config.Vis_Styles.Color_Access_Array_Access;
      Node_Icon_Id : Positive;                                         
      Color_Id : Positive;
      Attr_Filter : Giant.Graph_Lib.Node_Attribute_Filters.Filter;
   begin
   
      All_Icons  := Giant.Config.Vis_Styles.Get_All_Node_Icons;
      All_Colors := Giant.Config.Vis_Styles.Get_All_Colors;       
   
      Node_Icon_Id := Giant.Config.Vis_Styles.Get_Node_Icon_Encoding
        (Vis_Style, 
         Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class));              
     
      if not (Ada.Strings.Unbounded.To_String 
        (All_Icons.all (Node_Icon_Id)) = Req_Icon_File_Path) then
         
         Passed := False;
      end if;
      
      Color_Id := Config.Vis_Styles.Get_Border_Color 
        (Vis_Style, 
         Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class));
        
      if not (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
        Req_Border_Color) then

         Passed := False;
      end if;

                       
      Color_Id := Config.Vis_Styles.Get_Fill_Color 
        (Vis_Style, 
         Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class));
            
      if not (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
        Req_Fill_Color) then

         Passed := False;
      end if;
         
      Color_Id := Config.Vis_Styles.Get_Text_Color 
        (Vis_Style, 
         Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class));
                  
      if not (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
        Req_Text_Color) then

         Passed := False;
      end if;    
      
      
      Attr_Filter := Giant.Config.Vis_Styles.Get_Attribute_Filter 
        (Vis_Style, 
         Graph_Lib.Convert_Node_Class_Name_To_Id (Node_Class));
                   
      if not (Graph_Lib.Node_Attribute_Filters.Size (Attr_Filter) = 
        Attr_in_Filter) then

         Passed := False;
      end if;       
            
      return Passed;
   end Check_Node_Class_Config_Status;
      
   ---------------------------------------------------------------------------
   -- Test
   --------------------------------------------------------------------------- 
   
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
     
     All_Colors : Giant.Config.Vis_Styles.Color_Access_Array_Access;                                   
     Color_Id : Positive;     
   begin

      for i in 1 .. 1 loop

        Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
          ("",
           "resources/vis_styles/vis_styles_test_set_1/",
           "resources/vis_styles/vis_styles_test_set_2/",
           "resources/vis_styles/vis_styles_test_set_default_1/"
           & "test_vis_style_1_default.xml");
      
         Assert (Giant.Config.Vis_Styles.Get_Number_Of_Known_Vis_Styles = 4,
           "Test whether ammount of loaded vis styles is correct");
            
         Test_Vis_Style := 
           Giant.Config.Vis_Styles.Initialize_Vis_Style_By_Name
             ("test_vis_style_1_default");
                      
         Assert (Config.Vis_Styles.Get_Default_Vis_Style = Test_Vis_Style,
           "Check whether Default Vis Style Loaded");

         
         -- Check "test_vis_style_1_default"
         -----------------------------------
         
         -- check global data 
         --------------------
         All_Colors := Giant.Config.Vis_Styles.Get_All_Colors; 
         
         Color_Id := 
           Config.Vis_Styles.Get_Vis_Window_Background_Color (Test_Vis_Style);
         
         Assert 
           (Giant.Config.Get_Color_Value (All_Colors.all (Color_Id)) = 
            "RGB:20/20/20", "Test Correct Vis_Window_Background_Color");
            
         -- Check Node Class Data
         ------------------------   
                  
         -- check "TC_Floating_Point"
         Assert 
           (Check_Node_Class_Config_Status
             (Vis_Style          => Test_Vis_Style,
              Node_Class         => "TC_Floating_Point", 
              Req_Icon_File_Path => 
                File_Management.Get_Absolute_Path_To_File_From_Relative
                 ("./", 
                  "resources/vis_styles/vis_styles_test_set_default_1/"
                  & "test_node_icon_default_1.xpm"),      
              Req_Text_Color     => "RGB:AA/AA/A1",
              Req_Border_Color   => "RGB:AA/AA/A2",
              Req_Fill_Color     => "RGB:AA/AA/A3",      
              Attr_in_Filter     => 1),
            "Test config data for node class ""TC_Floating_Point""");               

         -- check "HPGNode"
         Assert 
           (Check_Node_Class_Config_Status
             (Vis_Style          => Test_Vis_Style,
              Node_Class         => "HPGNode", 
              Req_Icon_File_Path => 
                File_Management.Get_Absolute_Path_To_File_From_Relative
                  ("./", 
                   "resources/vis_styles/vis_styles_test_set_default_1/"
                   & "test_node_icon_blue_1.xpm"), 
              Req_Text_Color     => "blue",
              Req_Border_Color   => "blue",
              Req_Fill_Color     => "white",      
              Attr_in_Filter     => 1),
            "Test config data for node class ""HPGNode""");        

         -- check "TC_Boolean"
         Assert 
           (Check_Node_Class_Config_Status
             (Vis_Style          => Test_Vis_Style,
              Node_Class         => "TC_Boolean", 
              Req_Icon_File_Path => 
                File_Management.Get_Absolute_Path_To_File_From_Relative
                  ("./", 
                   "resources/vis_styles/vis_styles_test_set_default_1/"
                   & "test_node_icon_red_1.xpm"), 
              Req_Text_Color     => "red",
              Req_Border_Color   => "red",
              Req_Fill_Color     => "white",      
              Attr_in_Filter     => 2),
            "Test config data for node class ""TC_Boolean""");             
           
      end loop;
   end Test_Init_Test_Set_Default_1;
     
   ---------------------------------------------------------------------------
   -- Test Edge Data
   procedure Test_Init_Edge_Class_Vis_Data
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
     Test_Vis_Style : Giant.Config.Vis_Styles.Visualisation_Style_Access;         

   begin

      for i in 1 .. 1 loop

         Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
           ("",
            "resources/vis_styles/vis_styles_test_set_1/",
            "resources/vis_styles/vis_styles_test_set_2/",
            "resources/vis_styles/vis_styles_test_set_default_1/"
            & "test_vis_style_1_default.xml");
            
         Test_Vis_Style := 
           Giant.Config.Vis_Styles.Initialize_Vis_Style_By_Name
             ("test_vis_style_4_edges");                           
          
         -- test color for edge classes defined by default
         ---------------------------------------------------------------
    
         -- edge class "IML_Root.Parent"                      
         Assert 
           (Check_Edge_Class_Config_Status
             (Vis_Style      => Test_Vis_Style,
              Node_Class     => "IML_Root", 
              Attribute      => "Parent",
              Req_Line_Color => "RGB:AA/AA/A1",
              Req_Text_Color => "RGB:AA/AA/A2",
              Req_Line_Style => Giant.Config.Vis_Styles.Continuous_Line,
              Show_Label     => FALSE),
            "Test status of config data for edge class ""IML_Root.Parent""");

                        
         -- test color for edge classes defined by (start_node_class, *)
         ---------------------------------------------------------------------
         
         -- edge class "Routine_Call.EType"
         Assert 
           (Check_Edge_Class_Config_Status
             (Vis_Style      => Test_Vis_Style,
              Node_Class     => "Routine_Call", 
              Attribute      => "EType",
              Req_Line_Color => "blue",
              Req_Text_Color => "blue",
              Req_Line_Style => Giant.Config.Vis_Styles.Dotted_Line,
              Show_Label     => TRUE),
            "Test status of config data for edge class "
            & """Routine_Call.EType""");

         -- test color for edge classes defined by (*, attribute_name)
         ---------------------------------------------------------------------
         
         -- edge class "LR_Conversion.CF_Next"
         Assert 
           (Check_Edge_Class_Config_Status
             (Vis_Style      => Test_Vis_Style,
              Node_Class     => "LR_Conversion", 
              Attribute      => "CF_Next",
              Req_Line_Color => "blue",
              Req_Text_Color => "blue",
              Req_Line_Style => Giant.Config.Vis_Styles.Dotted_Line,
              Show_Label     => TRUE),
            "Test status of config data for edge class "
            & """LR_Conversion.CF_Next""");
            
         -- test color for edge classes defined by 
         -- (start_node_class, attribute_name)
         --------------------------------------------------------------------- 
         
         -- edge class Array_LR_Conversion.CF_Next           
         Assert 
           (Check_Edge_Class_Config_Status
             (Vis_Style      => Test_Vis_Style,
              Node_Class     => "Array_LR_Conversion", 
              Attribute      => "CF_Next",
              Req_Line_Color => "green",
              Req_Text_Color => "green",
              Req_Line_Style => Giant.Config.Vis_Styles.Dashed_Line,
              Show_Label     => FALSE),
            "Test status of config data for edge class "
            & """Array_LR_Conversion.CF_Next""");   
            

      end loop;
   end Test_Init_Edge_Class_Vis_Data;

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
      Register_Routine 
        (T, Test_Init_Edge_Class_Vis_Data'Access, 
         "Test_Init_Edge_Class_Vis_Data");                           
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
