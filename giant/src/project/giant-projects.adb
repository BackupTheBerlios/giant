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
--  $RCSfile: giant-projects.adb,v $, $Revision: 1.5 $
--  $Author: schwiemn $
--  $Date: 2003/06/16 07:34:08 $
--
with Bauhaus_IO; -- from Bauhaus IML "Reuse.src"

package body Giant.Projects is

   ---------------------------------------------------------------------------
   --  Name used for the file holding the node annotations
   Const_Node_Annotations_File_Name : constant String := 
     "node_annotations.xml";


   ---------------------------------------------------------------------------
   --  0.1
   --  Streaming functionality for platform independent streams.
   ---------------------------------------------------------------------------
 
   ---------------------------------------------------------------------------
   --  Does not read all parts of.3 the record !!! 
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
   --  Does not write all parts of the record !!!
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
   function Append_Dir_Separator_If_Necessary 
     (Directory : in String)     
     return String is     
      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;          
   begin               
      -- append directory separator if necessary
      if (Project_Directory (Project_Directory)'Last = Dir_Separator)
      
         return := Directory;
      else    
         
         return (Directory & Dir_Separator);
      end if;                
   end Append_Dir_Separator_If_necessary;
         
         
   ---------------------------------------------------------------------------
   function Calculate_Project_File_Name 
     (Project_Name      : in Valid_Names.Standard_Name;
      Project_Directory : in String) 
     return Ada.Strings.Unbounded.Unbounded_String is 
     
      use Ada.Strings.Unbounded;  
      
      Absolute_Project_Path := Ada.Strings.Unbounded.Unbounded_String          
   begin         
                  
      -- calculate absolute path for dir based on the current execution
      -- environment directory.
      -- Does no changes if "Project_Directory" already is an absolute path.
      Absolute_Project_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir,
           Project_Directory));
       
      -- build file name for project file
      return:= (Append_Dir_Separator_If_Necessary 
        (Ada.Strings.Unbounded.To_String (Absolute_Project_Path); 
        & Valid.Names.To_String (Project_Name)
        & ".xml";)
   end Calculate_Abs_Project_File_Name;

   ---------------------------------------------------------------------------
   function Load_Vis_Window_Into_Main_Memory (File_Path : String)
     return Vis_Windows.Visual_Window_Access is
     
      Stream_File       : Ada.Streams.Stream_IO.File_Type;  
      Ada_Stream        : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_In_Stream : Bauhaus_IO.In_Stream_Type;
      New_Vis_Window    : Vis_Windows.Visual_Window_Access;
   begin
      
      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.In_File,
         File_Path);
         
      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);    
      Bauhaus_In_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);
               
      Vis_Windows.Visual_Window_Access_Read;
        (Bauhaus_In_Stream, A_Subgraph_Data_Elemet.Subgraph);
                                     
      -- close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);      
      Ada.Streams.Stream_IO.Close (Stream_File);
      
      return Vis_Windows.Visual_Window_Access;
   end Load_Vis_Window_Into_Main_Memory;

   ---------------------------------------------------------------------------
   --  0.3
   --  Management of project related xml files 
   ---------------------------------------------------------------------------    
      
   ---------------------------------------------------------------------------
   --  Writes an external DTD into the directory where the xml project file 
   --  is located.
   procedure Write_DTD_To_Directory        
     (Project_Directory : in String) is
     
      use Ada.Strings.Unbounded;  
     
      Abs_DTD_File_Path : Ada.Strings.Unbounded.Unbounded_String;               
      Abs_DTD_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      DTD_File          : Ada.Text_IO.File_Type;
      
   begin
   
      --  calculate path name (make sure that a file name string could be
      --  appended)
      Abs_DTD_File_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir,
            Project_Directory));
      Abs_DTD_File_Name := Append_Dir_Separator_If_Necessary 
        (Ada.Strings.Unbounded.To_String (Abs_DTD_File_Path) 
        & Ada.Strings.Unbounded.To_Unbounded_String 
          ("giant_project_file.dtd"); 
                 
      Ada.Text_IO.Create 
        (DTD_File, 
         Ada.Text_IO.Out_File, 
         Ada.Strings.Unbounded.To_String (Abs_DTD_File_Name));                
      Ada.Text_IO.Set_Output(DTD_File);
      
      -- Write content of dtd file
      Ada.Text_IO.Put_Line       
        ("<!ELEMENT giant_project_file "
          &"(global_data, visualisation_windows, subgraphs)>");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line  
        ("  <!ELEMENT global_data EMPTY>");
      Ada.Text_IO.Put_Line 
        ("  <!ATTLIST global_data");
      Ada.Text_IO.Put_Line  
        ("    iml_graph_file_path        CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line          
        ("    iml_graph_checksum         CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line          
        ("    node_annotations_file_name CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line          
        ("  >");
        
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line  
        ("  <!ELEMENT visualisation_windows (a_vis_window_file)*>");
         
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line     
        ("    <!ELEMENT a_vis_window_file EMPTY>");
      Ada.Text_IO.Put_Line   
        ("    <!ATTLIST a_vis_window_file");
      Ada.Text_IO.Put_Line         
        ("      file_path  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line   
        ("    >");
               
	   Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("  <!ELEMENT subgraphs (a_subgraph_file)*>");
        
 	   Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line  
        ("    <!ELEMENT a_subgraph_file EMPTY>");
      Ada.Text_IO.Put_Line         
        ("    <!ATTLIST a_subgraph_file");
      Ada.Text_IO.Put_Line       
        ("      file_path  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line         
        ("    >");
  
      -- close resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (DTD_File);   
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
           (The_Project.Abs_Bauhaus_IML_Graph_File)
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
          (GNAT.Directory_Operations.Get_Current_Dir,
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
     
--     TODO

-- BEIM LADEN CHECKEN OB ZWEIMAL DER GLEICHE NAME VORKOMMT; FALLS JA
--EXCEPTION



     null;
   end Load_Project;
      
   ---------------------------------------------------------------------------
   function Create_Empty_Project
     (Project_Name                    : in Valid_Names.Standard_Name;
      Project_Directory               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access is
     
      New_Project_Access        : Project_Access; 
      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;
      
      Abs_Project_Directory     : Ada.Strings.Unbounded.Unbounded_String;
      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;
      Abs_IML_Graph_File        : Ada.Strings.Unbounded.Unbounded_String;
   begin
          
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;
    
      if Is_Already_A_Project_File_In_Directory (Project_Directory) then 
         raise Directory_Holds_Already_A_Project_File_Exception;
      end if;
      
      --  calculate absloute paths
      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir, Project_Directory));
          
      Abs_Project_Directory := Append_Dir_Separator_If_Necessary 
        (Ada.Strings.Unbounded.To_String (Abs_Project_Directory);
          
      Abs_Node_Annotations_File := Append_Dir_Separator_If_Necessary 
        (Ada.Strings.Unbounded.To_String (Abs_Project_Directory))
        & Ada.Strings.Unbounded.To_Unbounded_String       
          (Const_Node_Annotations_File_Name); 
          
      Abs_IML_Graph_File := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_File_From_Relative                      
         (GNAT.Directory_Operations.Get_Current_Dir, Bauhaus_IML_Graph_File));

      --  build new project                    
      New_Project_Access := new Project_Element;   
         
      New_Project_Access.Project_Name := 
        Ada.Strings.Unbounded.Unbounded_String 
          (Valid_Names.To_String (Project_Name));
           
      --  stored as an absolute path  (not written to the xml file)          
      New_Project_Access.Abs_Project_Dirctory := 
        Ada.Strings.Unbounded.Unbounded_String (Abs_Project_Directory)); 

      --  stored as an absolute path  (also written to the xml file)       
      New_Project_Access.Abs_Bauhaus_IML_Graph_File := 
        Ada.Strings.Unbounded.Unbounded_String (Abs_IML_Graph_File));  
           
      New_Project_Access.Bauhaus_IML_Graph_File_Checksum := 
        Bauhaus_IML_Graph_File_Checksum; 
  
      --  stored as an relative path (towards Project_Directory)
      --  - only while creating a new project (also written to xml file);         
      New_Project_Access.Node_Annotations_File :=
         Ada.Strings.Unbounded.To_Unbounded_String 
           (Const_Node_Annotations_File_Name);  
               
      New_Project_Access.All_Project_Vis_Windows := 
        Known_Vis_Windows_Hashs.Create;  
            
      New_Project_Access.All_Memory_Loaded_Vis_Windows := 
        Memory_Loaded_Vis_Window_Hashs.Create; 
             
      New_Project_Access.All_Subgraphs := 
        Subgraph_Data_Hashs.Create;    
          
      New_Project_Access.The_Node_Annotations := 
        Node_Annotations.Create_Empty;
                     
      --  write project files for new (empty) project into project directory
      --  (a xml project file and a dtd).
      Write_DTD_To_Directory  
        (Ada.Strings.Unbounded.To_String 
          (New_Project_Access.Project_Dirctory));          
      Write_Project_XML_File (New_Project_Access);
      
      --  write empty xml file for node annotations       
      Node_Annotations.Write_To_File
        (New_Project_Access.The_Node_Annotations,
         Abs_Node_Annotations_File);
            
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
   
   ----------------------------------------------------------------------------
   procedure Store_Whole_Project (Project : in Project_Access) is
   
      use Ada.Strings.Unbounded;
   
      Known_Vis_Iter            : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;
      
      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Elemet : Subgraph_Data_Elemet;               
      Subgraph_File_Name := Ada.Strings.Unbounded.Unbounded_String;      
      
      Stream_File : Ada.Streams.Stream_IO.File_Type;      
      Ada_Stream : Ada.Streams.Stream_IO.Stream_Access;      
      Bauhaus_Out_Stream :Bauhaus_IO.Out_Stream_Type;
   begin
   
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;
      
      -- write all memory loaded visualisation windows into management 
      -- files. All memory loaded vis windows will become "file linked".
      -----------------------------------------------------------------
      Known_Vis_Iter :=
        Known_Vis_Windows_Hashs.Make_Values_Iter 
          (Project.All_Project_Vis_Windows);
        
      while Known_Vis_Windows_Hashs.More (Known_Vis_Iter) loop 
          
         Known_Vis_Windows_Hashs.Next 
           (Known_Vis_Iter, A_Vis_Window_Data_Element);                              
         Store_Single_Project_Visualisation_Window
           (Project, A_Vis_Window_Data_Element.Vis_Window_Name);                   
      end loop;
      
      -- Write all iml_subgraphs into the management files and adjust
      -- status (make all of them file linked). 
      ---------------------------------------------------------------
      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (Project.All_Subgraphs);
                
      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop
            
         Subgraph_Data_Hashs.Next (Subgraphs_Iter, A_Subgraph_Data_Elemet);         

         -- Create new management File if necessary (loacted in project
         -- directory).
         if (A_Subgraph_Data_Elemet.Is_File_Linked = False) then
         
            -- Create Stream File         
            Subgraph_File_Name := Project.Abs_Project_Dirctory 
            & Graph_Lib.Subgraphs.Get_Name (A_Subgraph_Data_Elemet.Subgraph);
            & ".xml";
            
            -- just create the file
            Ada.Streams.Stream_IO.Create
              (Subgraph_Stream_File,
               Ada.Streams.Stream_IO.Out_File,
               Subgraph_File_Name);


-- TODO SUbgraphs loeschen !!!
               
            -- close resources   
            Ada.Streams.Stream_IO.Close (Subgraph_Stream_File);
                  
            A_Subgraph_Data_Elemet.Is_File_Linked := True;
            A_Subgraph_Data_Elemet.Existing_Subgraph_File :=
              Subgraph_File_Name;
         end if;  
         
         -- write Subgraph_Content into stream file     
         Subgraph_File_Name := A_Subgraph_Data_Elemet.Existing_Subgraph_File;
         Ada.Streams.Stream_IO.Open
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            Subgraph_File_Name);
         
         Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);    
         Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);
         
         -- stream the subgraph
         Graph_Lib.Subgraph_Write 
           (Bauhaus_Out_Stream, A_Subgraph_Data_Elemet.Subgraph);
                       
         -- close resources
         Bauhaus_IO.Release (Bauhaus_Out_Stream);      
         Ada.Streams.Stream_IO.Close (Stream_File);

      end loop;
      
      -- Write Node_Annotations
      -------------------------
      Node_Annotations.Write_To_File
        (Project.The_Node_Annotations,
         Project.Node_Annotations_File);
      
      -- Update Project XML File (MUST happen at the end - not before)
      ---------------------------
      Write_Project_XML_File (Project);      
   end Store_Whole_Project;
  
   ---------------------------------------------------------------------------
   --  Tricky Implementation working with status changes of visual window
   --  data elements and subgraph data elements.
   procedure Store_Whole_Project_As
     (Project           : in Project_Access;
      Project_Name      : in Valid_Names.Standard_Name;
      Project_Directory : in String) is
      
      use Ada.Strings.Unbounded;
      
      Abs_Project_Directory : Ada.Strings.Unbounded.Unbounded_String;  
      
      Known_Vis_Iter            : Known_Vis_Windows_Hashs.Values_Iter;
      A_Vis_Window_Data_Element : Vis_Window_Data_Element;    
      A_Vis_Window              : Vis_Windows.Visual_Window_Access;
      
      Subgraphs_Iter : Subgraph_Data_Hashs.Values_Iter;
      A_Subgraph_Data_Elemet : Subgraph_Data_Elemet;
   begin
   
      -- Security checks      
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;
      
      if (GNAT.OS_Lib.Is_Directory (Project_Directory) = False) then
         raise Invalid_Project_Directory_Excpetion;
      end if;
    
      if Is_Already_A_Project_File_In_Directory (Project_Directory) then 
         raise Directory_Holds_Already_A_Project_File_Exception;
      end if;   
                   
      ----------------------------
      --  calculate absloute paths for new files
      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir, Project_Directory));
                
      Abs_Project_Directory := Ada.Strings.Unbounded.To_Unbounded_String
        (Append_Dir_Separator_If_Necessary 
          (Ada.Strings.Unbounded.To_String (Abs_Project_Directory)));
            
      --  the new node annoations file    
      Abs_Node_Annotations_File := Abs_Project_Directory
        & Ada.Strings.Unbounded.To_Unbounded_String       
          (Const_Node_Annotations_File_Name);    
                              
      --  "Migrate the project" 
      --  (must be done befor processing vis windows and subgraphs).
      -------------------------
      Project.Project_Name := Ada.Strings.Unbounded.To_Unbounded_String
        (Valid_Names.To_String (Project_Name));
      
      Project.Abs_Project_Dirctory := Abs_Project_Directory;      
      Project.Node_Annotations_File := Abs_Node_Annotations_File;
                  
      --  change status of all visualisation windows
      --  and move all of them into the new directory  
      ----------------------------------------------- 
      Known_Vis_Iter :=
        Known_Vis_Windows_Hashs.Make_Values_Iter 
          (Project.All_Project_Vis_Windows);
        
      while Known_Vis_Windows_Hashs.More (Known_Vis_Iter) loop 
          
         Known_Vis_Windows_Hashs.Next 
           (Known_Vis_Iter, A_Vis_Window_Data_Element);                              

         if (A_Vis_Window_Data_Element.Is_Memory_Loaded = False) then
         
            -- load window into memory
            A_Vis_Window := Get_Visualisation_Window 
              (Project, 
               Valid_Names.To_Standard_Name
                 (Ada.Strings.Unbounded.To_String
                   (A_Vis_Window_Data_Element.Vis_Window_Name)));
                
            A_Vis_Window_Data_Element.Is_File_Linked := False;  
              A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
                Ada.Strings.Unbounded.Null_Unbounded_String;     
                  
            -- save window (as its status is "not file linked" a
            -- new file in the new project directory will be created
            -- (will change status to file linked)
            Store_Single_Project_Visualisation_Window 
              (Project,
               Valid_Names.To_Standard_Name
                 (Ada.Strings.Unbounded.To_String
                   (A_Vis_Window_Data_Element.Vis_Window_Name)));           
                    
            -- close vis window in project (does also deallocate)
            Close_Window_In_Project
              (Project,
               Valid_Names.To_Standard_Name
                 (Ada.Strings.Unbounded.To_String
                   (A_Vis_Window_Data_Element.Vis_Window_Name))); 
                              
         else              
            --  window is already memory loaded  
            A_Vis_Window := Get_Visualisation_Window 
              (Project, 
               Valid_Names.To_Standard_Name
                 (Ada.Strings.Unbounded.To_String
                   (A_Vis_Window_Data_Element.Vis_Window_Name)));
                
            A_Vis_Window_Data_Element.Is_File_Linked := False;  
              A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
                Ada.Strings.Unbounded.Null_Unbounded_String;     
                  
            -- save window (as its status is "not file linked" a
            -- new file in the new project directory will be created
            -- (will change status to file linked)
            Store_Single_Project_Visualisation_Window 
              (Project,
                Valid_Names.To_Standard_Name
                  (Ada.Strings.Unbounded.To_String
                    (A_Vis_Window_Data_Element.Vis_Window_Name)));           
         end if;                                   
      end loop;
                        
      --  change status of all subgraphs 
      --  this will result in moving all management files to the new 
      --  project directory on the next "store whole project" order.
      ---------------------------------------------------------------
      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
        (Project.All_Subgraphs);     
        
      while Subgraph_Data_Hashs.More (Subgraphs_Iter) loop
            
         Subgraph_Data_Hashs.Next (Subgraphs_Iter, A_Subgraph_Data_Elemet);
         A_Subgraph_Data_Elemet.Is_File_Linked := False;         
      end loop;
           
      -- write dtd
      Write_DTD_To_Directory  
        (Ada.S trings.Unbounded.To_String 
          (New_Project_Access.Project_Dirctory));
    
      -- Automatically writes the rest (xml file for
      -- node anntoations, the project xml file and management files
      -- for subgraphs) -
      -- (and the opended vis windows once more - that is not really
      --  necessary).
      Store_Whole_Project (Project);     
   end Store_Whole_Project_As;
 
   ---------------------------------------------------------------------------
   function Get_Project_Name
     (Project : in Project_Access)
     return String is
   begin
   
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;
          
      return Ada.Strings.Unbounded.To_String 
        (Project.Project_Name);
   end Get_Project_Name;

   ---------------------------------------------------------------------------
   function Get_Project_Directory
     (Project : in Project_Access)
     return Ada.Strings.Unbounded.Unbounded_String is
     
   begin
   
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;   
   
      return Ada.Strings.Unbounded.To_String 
        (Project.Abs_Project_Dirctory);        
   end Get_Project_Directory;
   
      
   ---------------------------------------------------------------------------
   -- B
   -- Visualisation Windows
   ---------------------------------------------------------------------------
   
   ---------------------------------------------------------------------------
   function Does_Vis_Window_Exist
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name)
     return Boolean is      
   begin
         
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;   
   
      return Known_Vis_Windows_Hashs.Is_Bound 
        (Project.All_Project_Vis_Windows, 
         Ada.Strings.Unbounded.To_Unbounded_String     
           (Valid_Names.To_String (Vis_Window_Name));
   end Does_Visualisation_Window_Exist;
   
   ---------------------------------------------------------------------------
   function Is_Vis_Window_Memory_Loaded
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name)
     return Boolean is
   begin  
   
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;   
   
      return Memory_Loaded_Vis_Window_Hashs.Is_Bound   
        (Project.All_Memory_Loaded_Vis_Windows, 
         Ada.Strings.Unbounded.To_Unbounded_String     
           (Valid_Names.To_String (Vis_Window_Name));        
   end Is_Vis_Window_Memory_Loaded;
                           
   ---------------------------------------------------------------------------
   function Get_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name)
     return Vis_Window_Management.Visual_Window_Access is
     
     Vis_Window_Data     : Vis_Window_Data_Element;
     New_Vis_Window_Inst : Vis_Windows.Visual_Window_Access;
               
   begin
     
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;  
      
      if (Does_Vis_Window_Exist (Project, Vis_Window_Name) = False)
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;
      
   
      --  Check if already loaded and return the instance if that is the case
      if Is_Vis_Window_Memory_Loaded (Project, Vis_Window_Name) = True) then
      
         return Memory_Loaded_Vis_Window_Hashs.Fetch         
           (Project.All_Memory_Loaded_Vis_Windows, 
            Ada.Strings.Unbounded.To_Unbounded_String     
             (Valid_Names.To_String (Vis_Window_Name));  
      --  load vis window into main memory
      else
         
         Vis_Window_Data := Known_Vis_Windows_Hashs.Fetch
           (Project.All_Project_Vis_Windows, 
            Ada.Strings.Unbounded.To_Unbounded_String     
              (Valid_Names.To_String (Vis_Window_Name));
         
         -- change data entry                  
         New_Vis_Window_Inst := 
           Load_Vis_Window_Into_Main_Memory 
             (Project,
              Ada.Strings.Unbounded.To_String
               (Vis_Window_Data.Existing_Vis_Window_File));
          
         -- insert into hash map
         Memory_Loaded_Vis_Window_Hashs.Bind 
           (Project.All_Memory_Loaded_Vis_Windows,
            Vis_Window_Data.Vis_Window_Name,
            New_Vis_Window_Inst);
                                                           
         -- update data entry
         Known_Vis_Windows_Hashs.Unbind 
           (Project.All_Project_Vis_Windows, 
            Ada.Strings.Unbounded.To_Unbounded_String     
              (Valid_Names.To_String (Vis_Window_Name));
                                           
         Vis_Window_Data.Is_Memory_Loaded := True

         Known_Vis_Windows_Hashs.Bind
           (Project.All_Project_Vis_Windows, 
            Ada.Strings.Unbounded.To_Unbounded_String     
              (Valid_Names.To_String (Vis_Window_Name)),
            Vis_Window_Data);
      
         retrun New_Vis_Window_Inst;
      end if;               
   end Get_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Add_Visualisation_Window
    (Project    : in Project_Access;
     Vis_Window : in Vis_Window_Management.Visual_Window_Access) is
     
     Vis_Window_Name : Ada.Strings.Unbounded.Unbounded_String;
     
   begin
   
      if (Project = null) then 
         raise Project_Access_Not_Initialized_Exception;
      end if;  
      
      if (Does_Vis_Window_Exist (Project, Vis_Window_Name) = True)
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;
   
  end Add_Visualisation_Window;
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
  
  
   -- !!!! Save a single visualisation window should write this window
   -- into the project file without changing the entries for the other
   -- windows
 
 
   
end Giant.Projects;






