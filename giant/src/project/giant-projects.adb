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
--  $RCSfile: giant-projects.adb,v $, $Revision: 1.3 $
--  $Author: schwiemn $
--  $Date: 2003/06/12 18:01:41 $
--
with Bauhaus_IO; -- from Bauhaus IML "Reuse.src"

package body Giant.Projects is

   ---------------------------------------------------------------------------
   --  Name used for the file holding the node annotations
   Node_Annotations_File : constant String := "node_annotations.xml";


   ---------------------------------------------------------------------------
   --  0.1
   --  Streaming functionality for platform independent streams.
   ---------------------------------------------------------------------------
 
   --------------------------------------------------------------------------- 
   procedure Subgraph_Data_Element_Read
     (Stream  : in     Bauhaus_IO.In_Stream_Type;
      Element :    out Subgraph_Data_Elemet) is 
      
      Highlight_Integer_Id : Integer;
   begin
   
      Giant.Graph_Lib.Subgraphs.Subgraph_Read (Stream, Element.Subgraph);
      
      --  Read Highlight Status
      Bauhaus_IO.Read_Integer (Stream, Highlight_Integer_Id);
      Element.Highlight_Status := 
        Subgraph_Highlight_Status'Val (Highlight_Integer_Id);   
   end Subgraph_Data_Elemet_Read;
 
   ---------------------------------------------------------------------------
   procedure Subgraph_Data_Element_Write
     (Stream  : in Bauhaus_IO.In_Stream_Type;
      Element : in Subgraph_Data_Elemet) is 
      
      Highlight_Integer_Id : Integer;
   begin
   
      Giant.Graph_Lib.Subgraphs.Subgraph_Write (Stream, Element.Subgraph);
      
      --  Write Highlight Status (via Conversion to integer)
      Highlight_Integer_Id := 
        Subgraph_Highlight_Status'Pos (Element.Highlight_Status);   
      Bauhaus_IO.Write_Integer (Stream, Highlight_Integer_Id);
   end Subgraph_Data_Elemet_Read;
         
         
   ---------------------------------------------------------------------------
   --  0.2
   --  Internal Subprograms
   ---------------------------------------------------------------------------       
         
   ---------------------------------------------------------------------------
   function Calculate_Abs_Project_File_Name 
     (Project_Name      : in Valid_Names.Standard_Name;
      Project_Directory : in String) 
     return Ada.Strings.Unbounded.Unbounded_String is 
     
      use Ada.Strings.Unbounded.Unbounded_String;
      
      Absolute_Project_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin         
         
      -- calculate absolute path for dir based on the current execution
      -- environment directory.
      -- Does no changes if "Project_Directory" already is an absolute path.
      Absolute_Project_Path := 
        File_Management.Get_Absolute_Path_To_File_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir,
           Project_Directory);
       
      -- build file name for project file
      return:= (Absolute_Project_Path 
        & Valid.Names.To_String (Project_Name)
        & ".xml";)
   end Calculate_Abs_Project_File_Name;


   ---------------------------------------------------------------------------
   --  0.3
   --  Management of project related xml files 
   ---------------------------------------------------------------------------    
      
   ---------------------------------------------------------------------------
   --  Writes an external DTD into the directory where the xml project file 
   --  is located.
   procedure Write_DTD_To_Directory        
     (Project_Directory : in String) is
               
      DTD_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      DTD_File : Ada.Text_IO.File_Type;
      
   begin
   
   TODO
   
--      -- Determine name and path for the DTD
--      DTD_File_Name := Ada.Strings.Unbounded."&"
--        (Ada.Strings.Unbounded.To_Unbounded_String
--          (File_Management.Return_Dir_Path_For_File_Path 
--            (Node_Annotations_File)),
--         "giant_node_annotations_file.dtd");
-             
--      Ada.Text_IO.Create 
--        (DTD_File, 
 --        Ada.Text_IO.Out_File, 
--         Ada.Strings.Unbounded.To_String (DTD_File_Name));                

--      Ada.Text_IO.Set_Output(DTD_File);
      
--      -- Write content of dtd file
--      Ada.Text_IO.Put_Line 
--        ("<!ELEMENT giant_node_annotations_file (node_annotation)*>");
--      Ada.Text_IO.Put_Line 
--        ("  <!ELEMENT node_annotation (#PCDATA)>");
--      Ada.Text_IO.Put_Line 
--        ("  <!ATTLIST node_annotation");       
--      Ada.Text_IO.Put_Line  
--        ("    node_id  ID  #REQUIRED");       
--      Ada.Text_IO.Put_Line       
--        ("  >");
  
--      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
--      Ada.Text_IO.Close (DTD_File);
   
   end Write_DTD_To_Directory;
   
   ---------------------------------------------------------------------------
   --  If you use this function to store a project you should first
   --  write the visualisation windows into the stream files, as this
   --  procedure only writes entries for vis windows into the 
   --  project file that are already "file linked" (same counts for 
   --  subgraphs).
   procedure Write_Project_XML_File (The_Project : Project_Access) is
   
      Project_File : Ada.Text_IO.File_Type;
      Abs_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;    
      
      Vis_Window_Iter : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      
      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Elemet : Subgraph_Data_Elemet;
      
   begin
   
      Abs_Project_File_Name :=
        Calculate_Abs_Project_File_Name 
          (Valid_Names.To_Standard_Name
            (Ada.Strings.Unbounded.To_String (The_Project.Project_Name)),
           Ada.Strings.Unbounded.To_String (The_Project.Project_Dirctory); 
   
      --  create the file
      -------------------
      Ada.Text_IO.Create 
        (Project_File, 
         Ada.Text_IO.Out_File, 
         Abs_Project_File_Name);              
      Ada.Text_IO.Set_Output (Project_File);   
      
      --  write xml file header for external dtd
      ------------------------------------------
      Ada.Text_IO.Put_Line 
        ("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");    
      Ada.Text_IO.Put_Line 
        ("<!DOCTYPE giant_project_file");
      Ada.Text_IO.Put_Line 
        ("  SYSTEM ""giant_project_file.dtd"">");
      Ada.Text_IO.New_Line;
      
      --  open top level xml node (document node)
      Ada.Text_IO.Put_Line 
        ("<giant_project_file>"); 
         
      --  write entry for global project data
      ---------------------------------------
      Ada.Text_IO.Put_Line 
        ("  <global_data");
      Ada.Text_IO.Put_Line
        ("    iml_graph_file_path = """
         & Ada.Strings.Unbounded.To_String 
           (The_Project.Bauhaus_IML_Graph_File)
         & """")
      Ada.Text_IO.Put_Line
        ("    iml_graph_checksum = """
         & Integer'Image (The_Project.Bauhaus_IML_Graph_File_Checksum) 
         & """")  
      Ada.Text_IO.Put_Line
        ("    node_annotations_file_name = """
         & Ada.Strings.Unbounded.To_String 
           (The_Project.Node_Annotations_File)
         & """ />")    
                
      --  write entries for the files holding the 
      --  streamed visualisation windows
      -------------------------------------------
      Ada.Text_IO.Put_Line 
        ("  <visualisation_windows>"); 

      --  iterate over all visualisation windows
      --  only writes windows that are FILE_Linked  !!!    
      Vis_Window_Iter := Known_Vis_Windows_Hashs.Make_Values_Iter 
        (The_Project.All_Project_Vis_Windows);
      
      while Known_Vis_Windows_Hashs.More (Vis_Window_Iter) loop
         Known_Vis_Windows_Hashs.Next 
           (Vis_Window_Iter, A_Vis_Window_Data_Element);
           
         if (A_Vis_Window_Data_Element.Is_File_Linked = True) then
         
            Ada.Text_IO.Put_Line 
              ("    <a_vis_window_file file_path = """
              & Ada.Strings.Unbounded.To_String 
                  (A_Vis_Window_Data_Element.Existing_Vis_Window_File)
              & """ />");         
         end if;  
      end loop;
            
      Ada.Text_IO.Put_Line 
        ("  </visualisation_windows>"); 
        
      --  write entries for the files holding the iml subgraphs
      ---------------------------------------------------------    
      Ada.Text_IO.Put_Line 
        ("  <subgraphs>");   
        
      Subgraphs_Iter : Subgraph_Data_Hashs.Make_Values_Iter
        (The_Project.All_Subgraphs);
        
      -- only writes entries for subgraphs that have an management file
      -- (and are "file linked") !!!
      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop
         Subgraph_Data_Hashs.Next 
           (Subgraphs_Iter, A_Subgraph_Data_Elemet);
           
         if (A_Subgraph_Data_Elemet.Is_File_Linked = True) then
         
            Ada.Text_IO.Put_Line 
              ("    <a_subgraph_file file_path = """
              & Ada.Strings.Unbounded.To_String 
                  (A_Subgraph_Data_Elemet.Existing_Subgraph_File)
              & """ />");         
         end if;             
      end loop;
       
      Ada.Text_IO.Put_Line 
        ("  </subgraphs>");   

      -- last entry in file - close top level xml node (document node)
      Ada.Text_IO.Put_Line 
        ("</giant_project_file>"); 
        
      --  close down resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Project_File);                     
   end Write_Project_XML_File;
   
                             
   ---------------------------------------------------------------------------
   -- A
   -- General Project Management
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Project_Exist
     (Project_Name : in Valid_Names.Standard_Name;
      Project_Directory : in String)
     return Boolean is
      
      Absolute_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String;          
      A_File : Ada.Text_IO.File_Type;           
      Project_Exists : Boolean := False;
      
      A_Tree_Reader  : Tree_Readers.Tree_Reader;
      A_XML_Document : Dom.Core.Document;      
   begin
   
      -- Check whether the directory exists       
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;

      -- calculate absolute path for dir based on the current execution
      -- environment directory.
      -- Does no changes if "Project_Directory" already is an absolute path.
      Absolute_Project_Path := 
        File_Management.Get_Absolute_Path_To_File_From_Relative
          (NAT.Directory_Operations.Get_Current_Dir,
           Project_Directory);
       
      -- build file name 
      Absolute_Project_File_Name := Calculate_Abs_Project_File_Name 
        (Project_Name, Project_Directory);
            
      -- check whether the xml file is a file that describes project
      begin 
         XML_File_Access.Load_XML_File_Validated
           (Ada.Strings.Unbounded.To_String 
             (Absolute_Project_File_NameFile_Name),
            A_Tree_Reader,
            A_XML_Document);
            
         -- check for project files
         if (XML_File_Access.Does_XML_Document_Belong_To_Type
           ("giant_project_file", A_XML_Document) = True) then
           
            Project_Exists := True;
         end if;      

         Tree_Readers.Free (A_Tree_Reader);                  
      exception
         when others =>
            Project_Exists := False;
      end;
                  
      return  Project_Exists;      
   end Does_Project_Exist;
 
   ---------------------------------------------------------------------------
   function Is_Already_A_Project_File_In_Directory
     (Project_Directory : in String)
     return Boolean is
      
      Project_File_Found : Boolean := False;
      
      File_List          : String_Lists.List;
      File_List_Iter     : String_Lists.ListIter;
      
      A_Tree_Reader      : Tree_Readers.Tree_Reader;
      A_XML_Document     : Dom.Core.Document;
      
      A_File_Name : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
          
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;
      
      -- process all xml files in the directory and check whether one is a 
      -- project file
      File_List := String_Lists.Create;
      
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
           (GIANT_Vis_Directory, True, ".xml"));
              
      File_List_Iter := String_Lists.MakeListIter (File_List);   
                                       
      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);

         begin 
            XML_File_Access.Load_XML_File_Validated
              (Ada.Strings.Unbounded.To_String (A_File_Name),
               A_Tree_Reader,
               A_XML_Document);
               
            -- check for project files
            if (XML_File_Access.Does_XML_Document_Belong_To_Type
              ("giant_project_file", A_XML_Document) = True) then
           
               Project_File_Found := True;
            end if;
         
            Tree_Readers.Free(A_Tree_Reader);  
         exception 
            when others =>
               null;
         end if;               
      end loop;
      
      String_Lists.Destroy (File_List);
      
      return Project_File_Found;   
   end Is_Already_A_Project_File_In_Directory;

   --------------------------------------------------------------------------
   procedure Get_Bauhaus_IML_Graph_Data
     (Project_Name           : in Valid_Names.Standard_Name;
      Project_Directory      : in String
      Bauhaus_IML_Graph_File : out Ada.Strings.Unbounded.Unbounded_String;
      Bauhaus_IML_Graph_File_Checksum : out Integer) is
      
      Absolute_Project_File_Name : Ada.Strings.Unbounded.Unbounded_String; 
      
      Project_Tree_Reader  : Tree_Readers.Tree_Reader;
      Project_XML_Document : Dom.Core.Document;
      
      XML_Nodes_List : DOM.Core.Node_List;
      Data_XML_Node  : DOM.Core.Node;      
   begin
  
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;
 
      if (Does_Project_Exist (Project_Name, Project_Directory) = False) then
         raise Project_Does_Not_Exist_Exception;
      end if;
      
      Absolute_Project_File_Name := Calculate_Abs_Project_File_Name 
        (Project_Name, Project_Directory);
      
      --  it is already certain that the file describes a project  
      XML_File_Access.Load_XML_File_Validated
        (Ada.Strings.Unbounded.To_String (A_File_Name),
         Project_Tree_Reader,
         Project_XML_Document);
     
      --  get the global setting node
      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (The_XML_Document, "global_data");

      --  the list holds only one node
      Data_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);
                          
      Bauhaus_IML_Graph_File := Ada.Strings.Unbounded.To_Unbounded_String
        (DOM.Core.Elements.Get_Attribute 
          (Data_XML_Node, "iml_graph_file_path"));  
      Bauhaus_IML_Graph_File_Checksum := 
        Ada.Strings.Unbounded.To_Unbounded_String
          (DOM.Core.Elements.Get_Attribute 
            (Data_XML_Node, "iml_graph_checksum"));           
                     
      --  deallocate storrage
      DOM.Core.Free (XML_Nodes_List);       
      Tree_Readers.Free(Project_Tree_Reader);  
   end Get_Bauhaus_IML_Graph_Data;
 
   ---------------------------------------------------------------------------
   function Load_Project
     (Project_Name : in Valid_Names.Standard_Name;
      Project_Directory : in String)
     return Project_Access is
     

   begin
     
     TODO
     null;
   end Load_Project;
      
   ---------------------------------------------------------------------------
   function Create_Empty_Project
     (Project_Name                    : in Valid_Names.Standard_Name;
      Project_Directory               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access is
     
      New_Project_Access : Project_Access; 
   begin
          
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;
    
      if Is_Already_A_Project_File_In_Directory (Project_Directory) then 
         raise Directory_Holds_Already_A_Project_File_Exception;
      end if;
                              
      New_Project_Access := new Project_Element;      
      New_Project_Access.Project_Name := 
        Ada.Strings.Unbounded.Unbounded_String 
          (Valid_Names.To_String (Project_Name));          
      New_Project_Access.Project_Dirctory := 
        Ada.Strings.Unbounded.Unbounded_String (Project_Directory)); 
                               
      New_Project_Access.Bauhaus_IML_Graph_File := 
        Ada.Strings.Unbounded.Unbounded_String (Bauhaus_IML_Graph_File));  
             
      New_Project_Access.Bauhaus_IML_Graph_File_Checksum := 
        Bauhaus_IML_Graph_File_Checksum; 
        
      New_Project_Access.Node_Annotations_File :=
         Ada.Strings.Unbounded.To_Unbounded_String (Node_Annotations_File);  
               
      New_Project_Access.All_Project_Vis_Windows := 
        Known_Vis_Windows_Hashs.Create;  
            
      New_Project_Access.All_Memory_Loaded_Vis_Windows := 
        Memory_Loaded_Vis_Window_Hashs.Create; 
             
      New_Project_Access.All_Subgraphs := 
        Subgraph_Data_Hashs.Create;    
          
      New_Project_Access.The_Node_Annotations := 
        Node_Annotations.Create_Empty;
                     
      --  write project files for new (empty) project into project directory
      TODO
      
      -- write empty xml file for node annotations 
    
       return New_Project_Access;
    end Create_Empty_Project;
    
   ----------------------------------------------------------------------------
   procedure Free_Project_Access is new Ada.Unchecked_Deallocation
     (Project_Element, Project_Access);
   
   ----------------------------------------------------------------------------
   procedure Deallocate_Project_Deep (Project : in out Project_Access) is
   
      Memory_Loeaded_Vis_Iter : Memory_Loaded_Vis_Window_Hashs.Values_Iter;
      A_Vis_Window_Access : Vis_Windows.Visual_Window_Access;
      
      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Elemet : Subgraph_Data_Elemet;
 
   begin       
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;
   
      --  deallocate All_Project_Vis_Windows
      --------------------------------------
      Known_Vis_Windows_Hashs.Destroy (Project.All_Project_Vis_Windows);
   
      --  deep deallocation of all memory loaded visualisation windows     
      ----------------------------------------------------------------
      Memory_Loeaded_Vis_Iter :=
        Memory_Loaded_Vis_Window_Hashs.Make_Values_Iter 
          (Project.All_Memory_Loaded_Vis_Windows);
        
      while Memory_Loaded_Vis_Window_Hashs.More (Memory_Loeaded_Vis_Iter) loop 
          
         Memory_Loaded_Vis_Window_Hashs.Next 
           (Memory_Loeaded_Vis_Iter, A_Vis_Window_Access);          
         Vis_Windows.Deallocate_Vis_Window_Deep (A_Vis_Window_Access);                     
      end loop;
      
      Memory_Loaded_Vis_Window_Hashs.Destroy 
        (Project.All_Memory_Loaded_Vis_Windows);
      
      --  deep deallocation of subgraphs
      ----------------------------------
      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (Project.All_Subgraphs);
        
      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop
            
         Subgraph_Data_Hashs.Next (Subgraphs_Iter, A_Subgraph_Data_Elemet);         
         Graph_Lib.Subgraphs.Destroy (A_Subgraph_Data_Elemet.Subgraph);
      end loop;
      
      Subgraph_Data_Hashs.Destroy (Project.All_Subgraphs);
      
      --  deep deallocation of the data structure for the node annotations
      --------------------------------------------------------------------
      Node_Annotations.Deallocate (Project.The_Node_Annotations);
      
      --  deallocate data object itself
      ---------------------------------
      Free_Project_Access (Project);
   end;           
end Giant.Projects;






