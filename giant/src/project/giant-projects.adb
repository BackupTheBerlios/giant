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
--  $RCSfile: giant-projects.adb,v $, $Revision: 1.12 $
--  $Author: squig $
--  $Date: 2003/06/17 16:08:41 $
--
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; -- from GNAT
with GNAT.Directory_Operations; -- from GNAT

with Bauhaus_IO; -- from Bauhaus IML "Reuse.src"

with Tree_Readers;       -- from xmlada
with Dom.Core;           -- from xmlada
with Dom.Core.Nodes;     -- from xmlada
with Dom.Core.Documents; -- from xmlada
with Dom.Core.Elements;  -- from xmlada

with Giant.File_Management; -- from GIANT
with Giant.XML_File_Access; -- from GIANT

package body Giant.Projects is

   ---------------------------------------------------------------------------
   --  Name used for the file holding the node annotations
   --  - only used when a new project is created.
   Const_Node_Annotations_File_Name : constant String :=
     "node_annotations.xml";

   ---------------------------------------------------------------------------
   --  Ending of Management file for a visual window
   Const_Vis_Window_File_Ending : constant String := ".viswin";

   --  Emergency files ending
   Const_Vis_Window_Emergency_File_Ending : constant String := ".viswin~";

   ---------------------------------------------------------------------------
   --  Ending of Management file for a subgraph
   Const_Subgraph_File_Ending : constant String := ".subgraph";

   --  Emergency files ending
   Const_Subgraph_Emergency_File_Ending : constant String := ".subgraph~";


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
   end Subgraph_Data_Element_Read;

   ---------------------------------------------------------------------------
   --  Does not write all parts of the record !!!
   procedure Subgraph_Data_Element_Write
     (Stream  : in Bauhaus_IO.Out_Stream_Type;
      Element : in Subgraph_Data_Elemet) is

      Highlight_Integer_Id : Integer;
   begin

      Giant.Graph_Lib.Subgraphs.Subgraph_Write (Stream, Element.Subgraph);

      --  Write Highlight Status (via Conversion to integer)
      Highlight_Integer_Id :=
        Subgraph_Highlight_Status'Pos (Element.Highlight_Status);
      Bauhaus_IO.Write_Integer (Stream, Highlight_Integer_Id);
   end Subgraph_Data_Element_Write;


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
      if (Directory (Directory'Last) = Dir_Separator) then

         return Directory;
      else

         return (Directory & Dir_Separator);
      end if;
   end Append_Dir_Separator_If_necessary;

   ---------------------------------------------------------------------------
   --  Used to create absolute file names (with absolute path) for
   --  files regarding the name and the ending.
   function Create_Name_For_File
     (Directory : in String;
      Name      : in String;
      Ending    : in String)
     return Ada.Strings.Unbounded.Unbounded_String is

      use Ada.Strings.Unbounded;

      Absolute_Dir_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- calculate absolute path for dir based on the current execution
      -- environment directory.
      -- Does no changes if "Directory" already is an absolute path.
      Absolute_Dir_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (File_Management.Get_Absolute_Path_To_Directory_From_Relative
          (GNAT.Directory_Operations.Get_Current_Dir,
           Directory));

      -- build file name for thefile
      return Ada.Strings.Unbounded.To_Unbounded_String
        (Append_Dir_Separator_If_Necessary
          (Ada.Strings.Unbounded.To_String (Absolute_Dir_Path))
         & Name
         & Ending);
   end Create_Name_For_File;

   ---------------------------------------------------------------------------
   --  Deletes all files in the directory
   --  with the corresponding endings regardeless of the content.
   procedure Kill_All_Emergency_Files (Directory : in String) is

      File_List      : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      File_List := String_Lists.Create;

      -- Emergency Files for subgraphs
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
           (Directory,
            True,
            Const_Subgraph_Emergency_File_Ending));

      -- Emergency Files for visualisation windows
      String_Lists.Attach
        (File_List,
         File_Management.Get_Filtered_Files_From_Directory
           (Directory,
            True,
            Const_Vis_Window_Emergency_File_Ending));

      File_List_Iter := String_Lists.MakeListIter (File_List);

      while String_Lists.More (File_List_Iter) loop

         String_Lists.Next (File_List_Iter, A_File_Name);
         File_Management.Delete_File (Ada.Strings.Unbounded.To_String
           (A_File_Name));
      end loop;

      String_Lists.Destroy (File_List);
   end Kill_All_Emergency_Files;

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

      Vis_Windows.Visual_Window_Access_Read
        (Bauhaus_In_Stream, New_Vis_Window);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);

      return New_Vis_Window;
   end Load_Vis_Window_Into_Main_Memory;

   ---------------------------------------------------------------------------
   procedure Write_Vis_Window_To_File
     (File_Path  : in String;
      Vis_Window : in Vis_Windows.Visual_Window_Access) is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_Out_Stream : Bauhaus_IO.Out_Stream_Type;
   begin

      -- check if file exists, create new one if necessary
      if (GNAT.OS_Lib.Is_Writable_File (File_Path) = True) then

         Ada.Streams.Stream_IO.Create
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);

         Ada.Streams.Stream_IO.Close (Stream_File);
      end if;

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.Out_File,
         File_Path);

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Vis_Windows.Visual_Window_Access_Write
           (Bauhaus_Out_Stream, Vis_Window);

      -- close resources
      Bauhaus_IO.Release (Bauhaus_Out_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);
   end Write_Vis_Window_To_File;

   ---------------------------------------------------------------------------
   function Load_Sub_Graph_Data_Into_Main_Memory (File_Path : String)
        return Subgraph_Data_Elemet is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_In_Stream  : Bauhaus_IO.In_Stream_Type;
      New_Sub_Graph_Data : Subgraph_Data_Elemet;
   begin

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.In_File,
         File_Path);

      Ada_Stream        := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_In_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Subgraph_Data_Element_Read
        (Bauhaus_In_Stream, New_Sub_Graph_Data);

      --  close resources
      Bauhaus_IO.Release (Bauhaus_In_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);

      --  -----
      New_Sub_Graph_Data.Is_File_Linked := True;
      New_Sub_Graph_Data.Existing_Subgraph_File :=
        Ada.Strings.Unbounded.To_Unbounded_String (File_Path);

      return New_Sub_Graph_Data;
   end Load_Sub_Graph_Data_Into_Main_Memory;

   ---------------------------------------------------------------------------
   procedure Write_Sub_Graph_Data_To_File
     (File_Path     : in String;
      Subgraph_Data : in Subgraph_Data_Elemet) is

      Stream_File        : Ada.Streams.Stream_IO.File_Type;
      Ada_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Bauhaus_Out_Stream : Bauhaus_IO.Out_Stream_Type;

   begin

      -- check if file exists, create new one if necessary
      if (GNAT.OS_Lib.Is_Writable_File (File_Path) = True) then

         Ada.Streams.Stream_IO.Create
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            File_Path);

         Ada.Streams.Stream_IO.Close (Stream_File);
      end if;

      Ada.Streams.Stream_IO.Open
        (Stream_File,
         Ada.Streams.Stream_IO.Out_File,
         File_Path);

      Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
      Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

      Subgraph_Data_Element_Write
        (Bauhaus_Out_Stream, Subgraph_Data);

      --  close resources
      Bauhaus_IO.Release (Bauhaus_Out_Stream);
      Ada.Streams.Stream_IO.Close (Stream_File);
   end Write_Sub_Graph_Data_To_File;


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

      Abs_DTD_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      DTD_File          : Ada.Text_IO.File_Type;
   begin

      --  calculate absolute dtd file name
      Abs_DTD_File_Name := Create_Name_For_File
        (Project_Directory,
         "giant_project_file",
         ".dtd");

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

      Abs_Project_File_Name := Create_Name_For_File
        (Ada.Strings.Unbounded.To_String (The_Project.Abs_Project_Dirctory),
         Ada.Strings.Unbounded.To_String (The_Project.Project_Name),
         ".xml");

      --  create the file
      -------------------
      Ada.Text_IO.Create
        (Project_File,
         Ada.Text_IO.Out_File,
         Ada.Strings.Unbounded.To_String (Abs_Project_File_Name));
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
         & """");
      Ada.Text_IO.Put_Line
        ("    iml_graph_checksum = """
         & Integer'Image (The_Project.Bauhaus_IML_Graph_File_Checksum)
         & """") ;
      -- node annotations file
      Ada.Text_IO.Put_Line
        ("    node_annotations_file_name = """
         & Ada.Strings.Unbounded.To_String
           (The_Project.Node_Annotations_File)
         & """ />");

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

      Subgraphs_Iter := Subgraph_Data_Hashs.Make_Values_Iter
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
     (Project_Name      : in String;
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

      -- build file name
      Absolute_Project_File_Name := Create_Name_For_File
        (Project_Directory,
         Project_Name,
         ".xml");

      -- check whether the xml file is a file that describes project
      begin
         XML_File_Access.Load_XML_File_Validated
           (Ada.Strings.Unbounded.To_String
             (Absolute_Project_File_Name),
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

      return Project_Exists;
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
           (Project_Directory, True, ".xml"));

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
         end;
      end loop;

      String_Lists.Destroy (File_List);

      return Project_File_Found;
   end Is_Already_A_Project_File_In_Directory;

   --------------------------------------------------------------------------
   procedure Get_Bauhaus_IML_Graph_Data
     (Project_Name           : in String;
      Project_Directory      : in String;
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

      Absolute_Project_File_Name := Create_Name_For_File
        (Project_Directory,
         Project_Name,
         ".xml");

      --  it is already certain that the file describes a project
      XML_File_Access.Load_XML_File_Validated
        (Ada.Strings.Unbounded.To_String (Absolute_Project_File_Name),
         Project_Tree_Reader,
         Project_XML_Document);

      --  get the global setting node
      XML_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Project_XML_Document, "global_data");

      --  the list holds only one node
      Data_XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, 0);

      Bauhaus_IML_Graph_File := Ada.Strings.Unbounded.To_Unbounded_String
        (DOM.Core.Elements.Get_Attribute
          (Data_XML_Node, "iml_graph_file_path"));
      Bauhaus_IML_Graph_File_Checksum :=
        Integer'Value (
          (DOM.Core.Elements.Get_Attribute
            (Data_XML_Node, "iml_graph_checksum")));

      --  deallocate storrage
      DOM.Core.Free (XML_Nodes_List);
      Tree_Readers.Free(Project_Tree_Reader);
   end Get_Bauhaus_IML_Graph_Data;

   ---------------------------------------------------------------------------
   function Load_Project
     (Project_Name      : in String;
      Project_Directory : in String)
     return Project_Access is


   begin

   --  TODO
   --  BEIM LADEN CHECKEN OB ZWEIMAL DER GLEICHE NAME VORKOMMT; FALLS JA
   --  EXCEPTION (bei Anzeigefenstern oder Subgraphen)



      return null;
   end Load_Project;

   ---------------------------------------------------------------------------
   function Create_Empty_Project
     (Project_Name                    : in String;
      Project_Directory               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access is

      use Ada.Strings.Unbounded;

      New_Project_Access        : Project_Access;
      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

      Abs_Project_Directory     : Ada.Strings.Unbounded.Unbounded_String;
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

      Abs_Project_Directory :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Append_Dir_Separator_If_Necessary
            (Ada.Strings.Unbounded.To_String (Abs_Project_Directory)));

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
        Ada.Strings.Unbounded.To_Unbounded_String (Project_Name);

      --  stored as an absolute path  (not written to the xml file)
      New_Project_Access.Abs_Project_Dirctory := Abs_Project_Directory;

      --  stored as an absolute path  (also written to the xml file)
      New_Project_Access.Abs_Bauhaus_IML_Graph_File :=
        Abs_IML_Graph_File;

      New_Project_Access.Bauhaus_IML_Graph_File_Checksum :=
        Bauhaus_IML_Graph_File_Checksum;

      --  Stored as an relative path (towards Project_Directory)
      --  - only while creating a new project (also written to xml file);
      --  Per default this file is loacted in the project directory.
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
          (New_Project_Access.Abs_Project_Dirctory));
      Write_Project_XML_File (New_Project_Access);

      --  write empty xml file for node annotations
      Node_Annotations.Write_To_File
        (New_Project_Access.The_Node_Annotations,
         Ada.Strings.Unbounded.To_String (Abs_Node_Annotations_File));

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
      Subgraph_File_Name : Ada.Strings.Unbounded.Unbounded_String;

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
            & Graph_Lib.Subgraphs.Get_Name (A_Subgraph_Data_Elemet.Subgraph)
            & ".xml";

            -- just create the file
            Ada.Streams.Stream_IO.Create
              (Stream_File,
               Ada.Streams.Stream_IO.Out_File,
               Ada.Strings.Unbounded.To_String (Subgraph_File_Name));



            Ada.Streams.Stream_IO.Close (Stream_File);

            A_Subgraph_Data_Elemet.Is_File_Linked := True;
            A_Subgraph_Data_Elemet.Existing_Subgraph_File :=
              Subgraph_File_Name;
         end if;

         -- write Subgraph_Content into stream file
         Subgraph_File_Name := A_Subgraph_Data_Elemet.Existing_Subgraph_File;
         Ada.Streams.Stream_IO.Open
           (Stream_File,
            Ada.Streams.Stream_IO.Out_File,
            Ada.Strings.Unbounded.To_String (Subgraph_File_Name));

         Ada_Stream := Ada.Streams.Stream_IO.Stream (Stream_File);
         Bauhaus_Out_Stream := Bauhaus_IO.Make_Internal (Ada_Stream);

         -- stream the subgraph
         Graph_Lib.Subgraphs.Subgraph_Write
           (Bauhaus_Out_Stream, A_Subgraph_Data_Elemet.Subgraph);

         -- close resources
         Bauhaus_IO.Release (Bauhaus_Out_Stream);
         Ada.Streams.Stream_IO.Close (Stream_File);

      end loop;

      -- Write Node_Annotations
      -------------------------
      Node_Annotations.Write_To_File
        (Project.The_Node_Annotations,
         Ada.Strings.Unbounded.To_String
           (Project.Node_Annotations_File));

      -- Update Project XML File (MUST happen at the end - not before)
      ---------------------------
      Write_Project_XML_File (Project);
   end Store_Whole_Project;

   ---------------------------------------------------------------------------
   --  Tricky Implementation working with status changes of visual window
   --  data elements and subgraph data elements.


   --    !!!TODO All Destroy Emergency Files (in old directory)
   procedure Store_Whole_Project_As
     (Project           : in Project_Access;
      Project_Name      : in String;
      Project_Directory : in String) is

      use Ada.Strings.Unbounded;

      Abs_Project_Directory     : Ada.Strings.Unbounded.Unbounded_String;

      Abs_Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

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
        (Project_Name);

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
               Ada.Strings.Unbounded.To_String
                 (A_Vis_Window_Data_Element.Vis_Window_Name));

            A_Vis_Window_Data_Element.Is_File_Linked := False;
              A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
                Ada.Strings.Unbounded.Null_Unbounded_String;

            -- save window (as its status is "not file linked" a
            -- new file in the new project directory will be created
            -- (will change status to file linked)
            Store_Single_Project_Visualisation_Window
              (Project,
               Ada.Strings.Unbounded.To_String
                 (A_Vis_Window_Data_Element.Vis_Window_Name));

            -- close vis window in project (does also deallocate)
            Close_Window_In_Project
              (Project,
               Ada.Strings.Unbounded.To_String
                 (A_Vis_Window_Data_Element.Vis_Window_Name));

         else
            --  window is already memory loaded
            A_Vis_Window := Get_Visualisation_Window
              (Project,
               Ada.Strings.Unbounded.To_String
                 (A_Vis_Window_Data_Element.Vis_Window_Name));

            A_Vis_Window_Data_Element.Is_File_Linked := False;
              A_Vis_Window_Data_Element.Existing_Vis_Window_File :=
                Ada.Strings.Unbounded.Null_Unbounded_String;

            -- save window (as its status is "not file linked" a
            -- new file in the new project directory will be created
            -- (will change status to file linked)
            Store_Single_Project_Visualisation_Window
              (Project,
               Ada.Strings.Unbounded.To_String
                 (A_Vis_Window_Data_Element.Vis_Window_Name));
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
        (Ada.Strings.Unbounded.To_String
          (Abs_Project_Directory));

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
     return String is

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
      Vis_Window_Name : in String)
     return Boolean is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Known_Vis_Windows_Hashs.Is_Bound
        (Project.All_Project_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));
   end Does_Vis_Window_Exist;

   ---------------------------------------------------------------------------
   function Is_Vis_Window_Memory_Loaded
     (Project         : in Project_Access;
      Vis_Window_Name : in String)
     return Boolean is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Memory_Loaded_Vis_Window_Hashs.Is_Bound
        (Project.All_Memory_Loaded_Vis_Windows,
         Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name));
   end Is_Vis_Window_Memory_Loaded;

   ---------------------------------------------------------------------------
   function Get_All_Visualisation_Window_Names
     (Project : in Project_Access)
     return String_Lists.List is

   begin

      -- Dummy
      return String_Lists.Create;
   end Get_All_Visualisation_Window_Names;

   ---------------------------------------------------------------------------
   function Get_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String)
     return Vis_Windows.Visual_Window_Access is

     Vis_Window_Data     : Vis_Window_Data_Element;
     New_Vis_Window_Inst : Vis_Windows.Visual_Window_Access;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Vis_Window_Exist (Project, Vis_Window_Name) = False) then
         raise Visualisation_Window_Is_Not_Part_Of_Project_Exception;
      end if;


      --  Check if already loaded and return the instance if that is the case
      if (Is_Vis_Window_Memory_Loaded (Project, Vis_Window_Name) = True) then

         return Memory_Loaded_Vis_Window_Hashs.Fetch
           (Project.All_Memory_Loaded_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String
             (Vis_Window_Name));
      --  load vis window into main memory
      else

         Vis_Window_Data := Known_Vis_Windows_Hashs.Fetch
           (Project.All_Project_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String
              (Vis_Window_Name));

         -- change data entry
         New_Vis_Window_Inst :=
           Load_Vis_Window_Into_Main_Memory
             (Ada.Strings.Unbounded.To_String
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
              (Vis_Window_Name));

         Vis_Window_Data.Is_Memory_Loaded := True;

         Known_Vis_Windows_Hashs.Bind
           (Project.All_Project_Vis_Windows,
            Ada.Strings.Unbounded.To_Unbounded_String (Vis_Window_Name),
            Vis_Window_Data);

         return New_Vis_Window_Inst;
      end if;
   end Get_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Add_Visualisation_Window
    (Project    : in Project_Access;
     Vis_Window : in Vis_Windows.Visual_Window_Access) is

      New_Vis_Window_Data : Vis_Window_Data_Element;
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Vis_Window_Exist
        (Project,
         Vis_Windows.Get_Name (Vis_Window)) = True) then

         raise Visualisation_Window_Is_Already_Part_Of_Project_Exception;
      end if;

      New_Vis_Window_Data.Is_File_Linked := False;
      New_Vis_Window_Data.Is_Memory_Loaded := True;
      New_Vis_Window_Data.Existing_Vis_Window_File :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      New_Vis_Window_Data.Vis_Window_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Vis_Windows.Get_Name (Vis_Window));

      Known_Vis_Windows_Hashs.Bind
        (Project.All_Project_Vis_Windows,
         New_Vis_Window_Data.Vis_Window_Name,
         New_Vis_Window_Data);

      Memory_Loaded_Vis_Window_Hashs.Bind
        (Project.All_Memory_Loaded_Vis_Windows,
         New_Vis_Window_Data.Vis_Window_Name,
         Vis_Window);
   end Add_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Store_Single_Project_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is

   begin
  -- DUMMY
      null;
   end Store_Single_Project_Visualisation_Window;

   ---------------------------------------------------------------------------
   procedure Close_Window_In_Project
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is

   begin
  -- DUMMY
      null;
   end Close_Window_In_Project;

   ---------------------------------------------------------------------------
   procedure Remove_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in String) is
   begin
   -- DUMMY
      null;
   end Remove_Visualisation_Window;


   ---------------------------------------------------------------------------
   -- C Subgraphs
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Subgraph_Exist
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Boolean is

   begin

      -- DUMMY
      return True;
   end Does_Subgraph_Exist;

   ---------------------------------------------------------------------------
   function Get_Subgraph
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Graph_Lib.Subgraphs.Subgraph is

   begin

      -- DUMMY
      return Graph_Lib.Subgraphs.Create(Subgraph_Name);
   end Get_Subgraph;


   ---------------------------------------------------------------------------
   function Get_All_Sungraphs
     (Project : in Project_Access)
     return String_Lists.List is
   begin

       -- DUMMY
      return String_Lists.Create;
   end Get_All_Sungraphs;

   ---------------------------------------------------------------------------
   procedure Add_Subgraph
     (Project      : in Project_Access;
      Subgraph : in Graph_Lib.Subgraphs.Subgraph) is

   begin
      null;
   end Add_Subgraph;

   ---------------------------------------------------------------------------
   procedure Remove_Subgraph
      (Project       : in Project_Access;
       Subgraph_Name : in String) is

   begin


     null;
   end Remove_Subgraph;

   ---------------------------------------------------------------------------
   function Get_Highlight_Status
     (Project       : in Project_Access;
      Subgraph_Name : in String)
     return Subgraph_Highlight_Status is

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Subgraph_Exist (Project, Subgraph_Name) = False) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      return Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String
           (Subgraph_Name)).Highlight_Status;
   end Get_Highlight_Status;

   ---------------------------------------------------------------------------
   procedure Change_Highlight_Status
     (Project              : in Project_Access;
      Subgraph_Name        : in String;
      New_Highlight_Status : in Subgraph_Highlight_Status) is

      A_Subgraph_Data_Element   : Subgraph_Data_Elemet;
      New_Subgraph_Data_Element : Subgraph_Data_Elemet;

   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      if (Does_Subgraph_Exist (Project, Subgraph_Name) = False) then
         raise Subgraph_Is_Not_Part_Of_Project_Exception;
      end if;

      A_Subgraph_Data_Element := Subgraph_Data_Hashs.Fetch
        (Project.All_Subgraphs,
         Ada.Strings.Unbounded.To_Unbounded_String (Subgraph_Name));

      -- change highlight status
      New_Subgraph_Data_Element := A_Subgraph_Data_Element;
      New_Subgraph_Data_Element.Highlight_Status :=
        New_Highlight_Status;

      Subgraph_Data_Hashs.Unbind
        (Project.All_Subgraphs,
          Ada.Strings.Unbounded.To_Unbounded_String
            (Graph_Lib.Subgraphs.Get_Name
              (A_Subgraph_Data_Element.Subgraph)));

      Subgraph_Data_Hashs.Bind
        (Project.All_Subgraphs,
          Ada.Strings.Unbounded.To_Unbounded_String
            (Graph_Lib.Subgraphs.Get_Name
              (New_Subgraph_Data_Element.Subgraph)),
                New_Subgraph_Data_Element);
   end Change_Highlight_Status;

   ---------------------------------------------------------------------------
   -- D Node Annotations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Node_Annotations
     (Project : in Project_Access)
     return Node_Annotations.Node_Annotation_Access is
   begin

      if (Project = null) then
         raise Project_Access_Not_Initialized_Exception;
      end if;

      return Project.The_Node_Annotations;
   end Get_Node_Annotations;

end Giant.Projects;






