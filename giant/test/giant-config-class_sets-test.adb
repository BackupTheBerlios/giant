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
--  $RCSfile: giant-config-class_sets-test.adb,v $, $Revision: 1.2 $
--  $Author: schwiemn $
--  $Date: 2003/07/10 21:57:33 $
--
with Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib;

with Giant.Logger;

package body Giant.Config.Class_Sets.Test is

   package Logger is new Giant.Logger("giant.config.class_sets.test");
   
      
   ---------------------------------------------------------------------------
   procedure Test_Init_Class_Sets
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
      All_Class_Sets_List : String_Lists.List; 
      
      Set_1 :  Config.Class_Sets.Class_Set_Access;            
      Set_2 :  Config.Class_Sets.Class_Set_Access; 
      Set_3 :  Config.Class_Sets.Class_Set_Access; 
      Set_4 :  Config.Class_Sets.Class_Set_Access;    
      
      A_Node_Class : Giant.Graph_Lib.Node_Class_id; 
      A_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;              
   begin

      for i in 1 .. 1 loop

         Config.Class_Sets.Initialize_Class_Sets 
           ("resources/class_sets/test_class_sets_1/");
                
         All_Class_Sets_List :=
           Config.Class_Sets.Get_All_Existing_Class_Sets;                      
         Assert 
           (String_Lists.Length (All_Class_Sets_List) = 4,
            "Test correct number of class sets loaded");
         String_Lists.Destroy (All_Class_Sets_List);
         
         
         -- Test single class set status
         Set_1 := Config.Class_Sets.Get_Class_Set_Access ("class_set_1");
         Set_2 := Config.Class_Sets.Get_Class_Set_Access ("class_set_2");         
         Set_3 := Config.Class_Sets.Get_Class_Set_Access ("class_set_3");
         Set_4 := Config.Class_Sets.Get_Class_Set_Access ("class_set_4");  
         
         -- class_set_1        
         Assert 
           (Config.Class_Sets.Is_Empty (Set_1),
            "Teste whether class_set_1 is empty");
                        
         -- class_set_2                        
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_2, A_Node_Class),
           "Test class_set_2 holds node class ""IML_Root""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_2, A_Node_Class),
           "Test class_set_2 holds node class ""HPGNode""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");        
         Assert 
           (not Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_2, A_Node_Class),
           "Test class_set_2 not holds node class ""SymNode"""); 
              
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("IML_Root", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_2, A_Edge_Class),
           "Test class_set_2 holds edge class ""IML_Root.Parent"""); 

         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("HPGNode", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_2, A_Edge_Class),
           "Test class_set_2 holds edge class ""HPGNode.Parent""");  
                     
         -- class_set_3
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_3, A_Node_Class),
           "Test class_set_3 holds node class ""SymNode""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Entity");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_3, A_Node_Class),
           "Test class_set_3 holds node class ""OC_Entity""");
           
        A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("HPGNode", "Parent"));
         Assert 
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_3, A_Edge_Class),
           "Test class_set_3 not holds edge class ""HPGNode.Parent""");  
         
         -- class_set_4
          A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_4, A_Node_Class),
           "Test class_set_4 holds node class ""T_Node""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Set_4, A_Node_Class),
           "Test class_set_4 holds node class ""O_Node""");
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("T_Node", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_4, A_Edge_Class),
           "Test class_set_4 holds edge class ""T_Node.Parent"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("O_Node", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_4, A_Edge_Class),
           "Test class_set_4 holds edge class ""O_Node.Parent"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Component"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Component", "Its_Type"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_4, A_Edge_Class),
           "Test class_set_4 holds edge class ""OC_Component.Its_Type"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Enum", "Its_Type"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_4, A_Edge_Class),
           "Test class_set_4 holds edge class ""OC_Enum.Its_Type"""); 
         
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Enum", "Parent"));
         Assert 
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Set_4, A_Edge_Class),
           "Test class_set_4 not holds edge class ""OC_Enum.Parent""");                  
                     
         -- deallocate                    
         Config.Class_Sets.Clear_Class_Sets;         
      end loop;
        
   end Test_Init_Class_Sets;
     
   ---------------------------------------------------------------------------
   procedure Test_Meta_Class_Set_Build
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
      All_Class_Sets_List : String_Lists.List; 
      
      S_Set_1 : Config.Class_Sets.Class_Set_Access;            
      S_Set_2 : Config.Class_Sets.Class_Set_Access; 
      S_Set_3 : Config.Class_Sets.Class_Set_Access; 
      S_Set_4 : Config.Class_Sets.Class_Set_Access;   
      
      Meta_1  : Config.Class_Sets.Meta_Class_Set_Access;
      Meta_E1 : Config.Class_Sets.Meta_Class_Set_Access;
      Meta_E2 : Config.Class_Sets.Meta_Class_Set_Access;
      
      Empty_Class_Set_List : Class_Sets_Lists.List;
      
      A_Node_Class : Giant.Graph_Lib.Node_Class_id; 
      A_Edge_Class : Giant.Graph_Lib.Edge_Class_Id;              
   begin
   
      Config.Class_Sets.Initialize_Class_Sets 
        ("resources/class_sets/test_class_sets_1/");
                                
      -- Test single class set status
      S_Set_1 := Config.Class_Sets.Get_Class_Set_Access ("class_set_1");
      S_Set_2 := Config.Class_Sets.Get_Class_Set_Access ("class_set_2");         
      S_Set_3 := Config.Class_Sets.Get_Class_Set_Access ("class_set_3");
      S_Set_4 := Config.Class_Sets.Get_Class_Set_Access ("class_set_4");  

      Empty_Class_Set_List := Class_Sets_Lists.Create;
      
      for i in 1 .. 50_000_000 loop
         
         Meta_1 := Config.Class_Sets.Build 
           (Elements => (S_Set_1, S_Set_2, S_Set_3, S_Set_4));
           
         Meta_E1 := Config.Class_Sets.Build 
           (Elements => Class_Set_Array'(1 .. 1 => S_Set_1));  

         Meta_E2 :=  Config.Class_Sets.Build (Empty_Class_Set_List);
         
        
         -- Test empty meta class sets
         -----------------------------
          
         Assert 
           (Config.Class_Sets.Is_Empty (Meta_E1),
            "Test whether Meta_E1 empty");
            
         Assert 
           (Config.Class_Sets.Is_Empty (Meta_E2),
            "Test whether Meta_E2 empty");         
                  
         -- Test Content of Meta_1
         --------------------------                                                   
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""IML_Root""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""HPGNode""");
                         
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("IML_Root"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("IML_Root", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""IML_Root.Parent"""); 

         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("HPGNode"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("HPGNode", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""HPGNode.Parent""");  
                     
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("SymNode");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""SymNode""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Entity");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""OC_Entity""");
                    
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node");           
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""T_Node""");
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node");        
         Assert 
           (Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 holds node class ""O_Node""");
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("T_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("T_Node", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""T_Node.Parent"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("O_Node"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("O_Node", "Parent"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""O_Node.Parent"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Component"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Component", "Its_Type"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""OC_Component.Its_Type"""); 
           
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Enum", "Its_Type"));
         Assert 
           (Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 holds edge class ""OC_Enum.Its_Type"""); 
         
         A_Edge_Class := 
           GIANT.Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
             (Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Enum"),
              Graph_Lib.Convert_Node_Attribute_Name_To_Id              
               ("OC_Enum", "Parent"));
         Assert 
           (not Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set 
             (Meta_1, A_Edge_Class),
           "Test Meta_1 not holds edge class ""OC_Enum.Parent"""); 
           
         A_Node_Class := 
           Graph_Lib.Convert_Node_Class_Name_To_Id ("OC_Record");        
         Assert 
           (not Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set 
             (Meta_1, A_Node_Class),
           "Test Meta_1 not holds node class ""OC_Record""");                 
                     
         Config.Class_Sets.Destroy (Meta_1);                      
         Config.Class_Sets.Destroy (Meta_E1);   
         Config.Class_Sets.Destroy (Meta_E2);   
      end loop;        

      Config.Class_Sets.Clear_Class_Sets; 
      Class_Sets_Lists.Destroy (Empty_Class_Set_List);  
   end Test_Meta_Class_Set_Build;

   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Config.Class_Sets.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine 
        (T, Test_Init_Class_Sets'Access, "Test_Init_Class_Sets");
      Register_Routine 
        (T, Test_Meta_Class_Set_Build'Access, "Test_Meta_Class_Set_Build");              
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Initialize;      
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Giant.Graph_Lib.Destroy;
   end Tear_Down;

end Giant.Config.Class_Sets.Test;