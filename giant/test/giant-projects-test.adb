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
--  $RCSfile: giant-projects-test.adb,v $, $Revision: 1.12 $
--  $Author: schwiemn $
--  $Date: 2003/07/15 23:40:17 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Gtk.Main;

with Gnat.OS_Lib;

with Giant.Config.Vis_Styles;
with Giant.File_Management;
with Giant.Logger;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Vis_Windows;
with Giant.Node_Annotations;

with Giant.Projects;

package body Giant.Projects.Test is

   package Logger is new Giant.Logger("Giant.Projects.Test");

   ---------------------------------------------------------------------------
   --  Deletes all files in the directory
   procedure Kill_Files_In_Dir (Directory : in String) is

      File_List      : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      File_List := String_Lists.Create;

      -- Security Files for visualisation windows
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
         (Directory,
          False,
          ""));

      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);
         File_Management.Delete_File (Ada.Strings.Unbounded.To_String
                                      (A_File_Name));
      end loop;

      String_Lists.Destroy (File_List);
   end Kill_Files_In_Dir;
   
   ---------------------------------------------------------------------------
   function Create_Test_Project_1 return Giant.Projects.Project_Access is 
   
      Test_Project_1 : Giant.Projects.Project_Access;
      A_Subgraph     : Giant.Graph_Lib.Subgraphs.Subgraph;  
      A_Vis_Window   : Giant.Vis_Windows.Visual_Window_Access;
      
   begin    
         
      Test_Project_1 := Create_Empty_Project
        (Project_Name => "Test_Project_1",
         Project_Directory => "resources/test_project_directory/dir_1",
         Bauhaus_IML_Graph_File => "resources/rfg_examp.iml",
         Bauhaus_IML_Graph_File_Checksum => Giant.Graph_Lib.Get_Graph_Hash);
            
      -- add subgraphs
      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_1");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);   
        
      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_2");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);         

      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_3");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);  

      A_Subgraph := Giant.Graph_Lib.Subgraphs.Create ("Sub_Graph_4");
      Giant.Projects.Add_Subgraph (Test_Project_1, A_Subgraph);  

      -- add vis windows
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_1");
      
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_2");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_3");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);
        
      A_Vis_Window := Giant.Vis_Windows.Create_New ("Vis_Window_4");
      Giant.Projects.Add_Visualisation_Window
        (Test_Project_1, A_Vis_Window);

      return Test_Project_1;
   end Create_Test_Project_1;
                         
   ---------------------------------------------------------------------------
   procedure Check_Test_Project_1       
     (Project : in Giant.Projects.Project_Access) is 
   
     Test_List : String_Lists.List;
   begin
   
      -- check project status
      Assert 
        (Projects.Get_Project_Name (Project) = "Test_Project_1",
         "Check correct project name after initialisation");

      -- check subgraphs
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_1"),
         "Check whether Sub_Graph_1 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_1"),
         "Check whether Sub_Graph_2 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_1"),
         "Check whether Sub_Graph_3 exits");
         
      Assert (Projects.Does_Subgraph_Exist
        (Project, "Sub_Graph_1"),
         "Check whether Sub_Graph_4 exits");

      Assert (not Projects.Does_Subgraph_Exist
        (Project, "Donald"),
         "Check whether Sub_Graph Donald not exits");
         
      Test_List := 
        Projects.Get_All_Subgraphs (Project);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of Subgraph Names list");
      String_Lists.Destroy (Test_List);   


      -- check vis windows
      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_1"),
         "Check whether Vis_Window_1 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_2"),
         "Check whether Vis_Window_2 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_3"),
         "Check whether Vis_Window_3 exists");

      Assert (Projects.Does_Vis_Window_Exist
        (Project, "Vis_Window_4"),
         "Check whether Vis_Window_4 exists");

      Assert (not Projects.Does_Vis_Window_Exist
        (Project, "Durchsicht"),
         "Check whether Vis_Window Durchsicht not exists");  
      
      Test_List := 
        Projects.Get_All_Visualisation_Window_Names (Project);
      Assert (String_Lists.Length (Test_List) = 4,
              "Test Lenght of Vis Window Names list");
      String_Lists.Destroy (Test_List);
   
   end Check_Test_Project_1;
   
   ---------------------------------------------------------------------------
   procedure Check_Whether_All_T_Project_1_Files_Written
     (Dir : in String) is
     
      Test_List : String_Lists.List;      
   begin
       
      -- check dir
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        (Dir,
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 12,
         "Test correct ammount of files in dir_1 - new project created");
      String_Lists.Destroy (Test_List);   
      
      -- project files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir & "Test_Project_1.xml"),
         "Test Project_File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir & "/giant_project_file.dtd"),
         "Test Project DTD File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "node_annotations.xml"),
         "Test Node Annotartions File written correctly");                  
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "giant_node_annotations_file.dtd"),
         "Test Node Annotartions DTD File written correctly");
      
      -- Subgraph Stream files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_1.subgraph"),
         "Test Sub_Graph_1 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_2.subgraph"),
         "Test Sub_Graph_2 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_3.subgraph"),
         "Test Sub_Graph_3 File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_4.subgraph"),
         "Test Sub_Graph_4 File written correctly");
         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Sub_Graph_4.subgraph"),      
         "Test Sub_Graph_4 File written correctly");
     
      -- Vis Window Stream files
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_1.viswin"),
         "Test Vis_Window_1 File written correctly");         
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_2.viswin"),
         "Test Vis_Window_2 File written correctly");   
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_3.viswin"),
         "Test Vis_Window_3 File written correctly");   
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          (Dir
           & "Vis_Window_4.viswin"),
         "Test Vis_Window_4 File written correctly");   
   
   end Check_Whether_All_T_Project_1_Files_Written;





   ---------------------------------------------------------------------------
   procedure Test_Initialize
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;

   begin

      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");      
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);     
      Projects.Deallocate_Project_Deep (Test_Project_1);
   end Test_Initialize;


   ---------------------------------------------------------------------------
   procedure Basic_File_Mangement_Test
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Project_1 : Giant.Projects.Project_Access;
      Test_List      : String_Lists.List;      

   begin
   
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
      -- test directory content after initialisation
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/Test_Project_1.xml"),
         "Test Project_File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/giant_project_file.dtd"),
         "Test Project DTD File written correctly");
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "node_annotations.xml"),
         "Test Node Annotartions File written correctly");                  
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/"
           & "giant_node_annotations_file.dtd"),
         "Test Node Annotartions DTD File written correctly");
              
      Test_List := File_Management.Get_Filtered_Files_From_Directory
        ("resources/test_project_directory/dir_1/",
         False,
         "");
      Assert 
        (String_Lists.Length (Test_List) = 4,
         "Test correct ammount of files in dir_1 - new project created");
      String_Lists.Destroy (Test_List);   

      -- test content after storing 
      Projects.Store_Whole_Project (Test_Project_1);
      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_1/");
                
      -- advanced storing test
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1.xml");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");
                  
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As_For_File
      (Test_Project_1,
       "resources/test_project_directory/dir_2/Test_Project_1");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");        
        
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");      
      Projects.Store_Whole_Project_As
      (Test_Project_1,
       "Test_Project_1",
       "resources/test_project_directory/dir_2/");      
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_2/");   
        
      Check_Whether_All_T_Project_1_Files_Written
        ("resources/test_project_directory/dir_1/");
        
        
        
      -- check project directoty content test                              
      Projects.Deallocate_Project_Deep (Test_Project_1);
      
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Test_Project_1 := Create_Test_Project_1;            
      Check_Test_Project_1 (Test_Project_1);
      
      Assert
        (Gnat.OS_Lib.Is_Regular_File
          ("resources/test_project_directory/dir_1/Test_Project_1.xml"),
         "Test Project_File written correctly");
                 
      Assert 
        (Projects.Does_Project_Exist
          ("Test_Project_1",
           "resources/test_project_directory/dir_1"),
         "Check Procedure Does_Project_Exist - project found");



         
      File_Management.Delete_File  
        ("resources/test_project_directory/dir_1/Test_Project_1.xml");
        
      Assert 
        (not Projects.Does_Project_Exist
          ("Test_Project_1",
           "resources/test_project_directory/dir_1"),
         "Check Procedure Does_Project_Exist - project notfound");
      
      Projects.Deallocate_Project_Deep (Test_Project_1);   
         
   end Basic_File_Mangement_Test;
   

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.Projects - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Initialize'Access, "Inititialisation");
      Register_Routine 
        (T, Basic_File_Mangement_Test'Access, "Basic_File_Mangement_Test");      
                  
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin

      Gtk.Main.Init;
      
      Giant.Graph_Lib.Initialize;

      Giant.Graph_Lib.Load
        ("resources/rfg_examp.iml");

      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("",
         "resources/vis_styles/vis_styles_test_set_1",
         "",
         "resources/vis_styles/vis_styles_test_set_1/"
         & "test_vis_style_1_default.xml");
         
      -- create directories necessary for test (if they do not already exist)
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_1");
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_2");                 
      File_Management.Create_Dir_Path 
        ("resources/test_project_directory/dir_3");       
        
      Kill_Files_In_Dir ("resources/test_project_directory/dir_1");
      Kill_Files_In_Dir ("resources/test_project_directory/dir_2");
      Kill_Files_In_Dir ("resources/test_project_directory/dir_3");
        
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
   
      Giant.Graph_Lib.Destroy;
      Giant.Config.Vis_Styles.Clear_Config_Vis_Styles;
   end Tear_Down;

end Giant.Projects.Test;
