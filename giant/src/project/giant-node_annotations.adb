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
--  $RCSfile: giant-node_annotations.adb,v $, $Revision: 1.8 $
--  $Author: schwiemn $
--  $Date: 2003/06/10 12:34:46 $
--
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GIANT.File_Management; --  from GIANT
with GIANT.XML_File_Access; --  from GIANT

with Tree_Readers;       --  from xmlada
with DOM.Core;           --  from xmlada
with DOM.Core.Documents; --  from xmlada
with DOM.Core.Nodes;     --  from xmlada
with DOM.Core.Elements;  --  from xmlada

package body Giant.Node_Annotations is


   ---------------------------------------------------------------------------
   -- 0.1
   -- Internal subprograms
   ---------------------------------------------------------------------------
   
   ---------------------------------------------------------------------------
   --  Writes a DTD into the directory where the xml file for node
   --  annotations is located.
   procedure Write_DTD_To_Directory        
     (Node_Annotations_File : in String) is
               
      DTD_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      DTD_File : Ada.Text_IO.File_Type;
      
   begin
   
      -- Determine name and path for the DTD
      DTD_File_Name := Ada.Strings.Unbounded."&"
        (Ada.Strings.Unbounded.To_Unbounded_String
          (File_Management.Return_Dir_Path_For_File_Path 
            (Node_Annotations_File)),
         "giant_node_annotations_file.dtd");
             
      Ada.Text_IO.Create 
        (DTD_File, 
         Ada.Text_IO.Out_File, 
         Ada.Strings.Unbounded.To_String (DTD_File_Name));                

      Ada.Text_IO.Set_Output(DTD_File);
      
      -- Write content of dtd file
      Ada.Text_IO.Put_Line 
        ("<!ELEMENT giant_node_annotations_file (node_annotation)*>");
      Ada.Text_IO.Put_Line 
        ("  <!ELEMENT node_annotation (#PCDATA)>");
      Ada.Text_IO.Put_Line 
        ("  <!ATTLIST node_annotation");       
      Ada.Text_IO.Put_Line  
        ("    node_id  ID  #REQUIRED");       
      Ada.Text_IO.Put_Line       
        ("  >");
  
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (DTD_File);
   
   end Write_DTD_To_Directory;


   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, persistence and deallocation.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Load_From_File
     (Node_Annotations_File : in String)
     return Node_Annotation_Access is

      New_Node_Annotation_Access : Node_Annotation_Access := null;

      Annotation_Tree_Reader : Tree_Readers.Tree_Reader;
      Annotation_XML_Document : Dom.Core.Document;

      XML_Annotation_Nodes_List : DOM.Core.Node_List;
      XML_Annotation_Node : DOM.Core.Node;
      XML_Annotation_Text_Node : DOM.Core.Node;

      A_Node_Id : Graph_Lib.Node_Id;

   begin

      --  access and check the xml file
      --------------------------------
      begin

         XML_File_Access.Load_XML_File_Validated
           (Node_Annotations_File,
            Annotation_Tree_Reader,
            Annotation_XML_Document);

      exception
         when XML_File_Access.XML_File_Access_Error_Exception =>
            raise Node_Annotations_File_Not_Found_Exception;

         when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
            raise Node_Annotations_File_Not_Correct_Exception;
      end;

      if (XML_File_Access.Does_XML_Document_Belong_To_Type
           ("giant_node_annotations_file", 
            Annotation_XML_Document) = False) then
            
         Tree_Readers.Free (Annotation_Tree_Reader);
         raise Node_Annotations_File_Not_Correct_Exception;
      end if;

      --  initialize empty New_Node_Annotation_Access
      -----------------------------------------------
      New_Node_Annotation_Access := Create_Empty;

      --  process the xml file
      ------------------------
      XML_Annotation_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Annotation_XML_Document, "node_annotation");

      for I in 0 .. DOM.Core.Nodes.Length
        (XML_Annotation_Nodes_List) - 1 loop

         XML_Annotation_Node := DOM.Core.Nodes.Item
           (XML_Annotation_Nodes_List, I);

         -- the text node that holds the annotation
         XML_Annotation_Text_Node := DOM.Core.Nodes.First_Child
           (XML_Annotation_Node);

         -- check whether the iml node does exist
         -- ignore not existing nodes
         if Graph_Lib.Does_Node_Id_Exist
           (DOM.Core.Elements.Get_Attribute
            (XML_Annotation_Node, "node_id")) then

            A_Node_Id := Graph_Lib.Node_Id_Value
              (DOM.Core.Elements.Get_Attribute
               (XML_Annotation_Node, "node_id"));

            -- for each node there may be only one annotation
            Node_Annotation_Hashed_Mappings.Bind
              (New_Node_Annotation_Access.Annotations,
               A_Node_Id,
               Ada.Strings.Unbounded.To_Unbounded_String
                 (DOM.Core.Nodes.Node_Value (XML_Annotation_Text_Node)));
         end if;
      end loop;

      -- deallocate resources
      DOM.Core.Free (XML_Annotation_Nodes_List);
      Tree_Readers.Free (Annotation_Tree_Reader);

      return New_Node_Annotation_Access;
   end Load_From_File;

   ---------------------------------------------------------------------------
   function Create_Empty return Node_Annotation_Access is

      New_Node_Annotation_Access : Node_Annotation_Access := null;
   begin

      --  initialize empty New_Node_Annotation_Access
      -----------------------------------------------
      New_Node_Annotation_Access := new Node_Annotation_Element;
      New_Node_Annotation_Access.Annotations :=
        Node_Annotation_Hashed_Mappings.Create;

      return New_Node_Annotation_Access;
   end Create_Empty;

   ---------------------------------------------------------------------------
   procedure Write_To_File
     (Node_Annotations          : in Node_Annotation_Access;
      Node_Annotations_File     : in String) is

      The_File : Ada.Text_IO.File_Type;
      
      Annotations_Iter  : Node_Annotation_Hashed_Mappings.Bindings_Iter;
      Annotation_Text   : Ada.Strings.Unbounded.Unbounded_String;
      Annotated_Node_ID : Graph_Lib.Node_Id;

   begin

      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if;

      -- try to create the file
      begin

         Ada.Text_IO.Create 
           (The_File, 
            Ada.Text_IO.Out_File, 
            Node_Annotations_File);                       
      exception
         when others =>
            raise Node_Annotations_File_Could_Not_Be_Written_Exception;         
      end;
  
      Ada.Text_IO.Set_Output(The_File);

      -- File Header
      Ada.Text_IO.Put_Line 
        ("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");    
      Ada.Text_IO.Put_Line 
        ("<!DOCTYPE giant_node_annotations_file");
      Ada.Text_IO.Put_Line 
        ("  SYSTEM ""giant_node_annotations_file.dtd"">");
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line 
        ("<giant_node_annotations_file>");
        
      -- Write all known node annotations
      Annotations_Iter := Node_Annotation_Hashed_Mappings.Make_Bindings_Iter 
        (Node_Annotations.Annotations);
      
      while Node_Annotation_Hashed_Mappings.More (Annotations_Iter) loop
         Node_Annotation_Hashed_Mappings.Next
           (Annotations_Iter, 
            Annotated_Node_ID, 
            Annotation_Text);
            
         -- Write entry for an annotaded node  
         Ada.Text_IO.Put
           ("<node_annotation node_id=""");
         Ada.Text_IO.Put         
           (Graph_Lib.Node_Id_Image (Annotated_Node_ID));
         Ada.Text_IO.Put_Line (""">");
                  
         Ada.Text_IO.Put_Line
           (Ada.Strings.Unbounded.To_String (Annotation_Text));
         
         Ada.Text_IO.Put_Line 
           ("</node_annotation>");
         Ada.Text_IO.New_Line;
      end loop;
  
      Ada.Text_IO.Put_Line 
        ("</giant_node_annotations_file>");  
  
      -- Put a DTD into the same directory as the xml file just written
      Write_DTD_To_Directory (Node_Annotations_File);
        
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (The_File);      
   end Write_To_File;

   ---------------------------------------------------------------------------
   procedure Free_Node_Annotation_Access is new Ada.Unchecked_Deallocation
     (Node_Annotation_Element, Node_Annotation_Access);
      
   procedure Deallocate
     (Node_Annotations : in out Node_Annotation_Access) is
   begin
      
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if;

      --  Free Hash Map
      Node_Annotation_Hashed_Mappings.Destroy (Node_Annotations.Annotations);
      
      --  Free pointer
      Free_Node_Annotation_Access (Node_Annotations);   
   end Deallocate;


   ---------------------------------------------------------------------------
   --  B
   --  Node Annotations - Read Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Is_Annotated
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
     return Boolean is
     
   begin
   
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if;
   
      return Node_Annotation_Hashed_Mappings.Is_Bound
        (Node_Annotations.Annotations, Node);
   end Is_Annotated;

   ---------------------------------------------------------------------------
   function Get_Annotation_Text
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
     return String is
   begin
   
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
      
      if (Is_Annotated (Node_Annotations, Node) = False) then
         raise Node_Is_Not_Annotated_Exception;
      end if;
      
      return Ada.Strings.Unbounded.To_String 
        (Node_Annotation_Hashed_Mappings.Fetch
          (Node_Annotations.Annotations, Node));
   end Get_Annotation_Text;

   ---------------------------------------------------------------------------
   function Get_All_Annotated_Nodes 
     (Node_Annotations : in Node_Annotation_Access) 
     return Graph_Lib.Node_Id_Lists.List is
     
      The_List : Graph_Lib.Node_Id_Lists.List;       
      Iter : Node_ID_Iter;
      A_Node : Graph_Lib.Node_Id;
      
   begin
   
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
         
      The_List := Graph_Lib.Node_Id_Lists.Create;      
      Iter := Make_Node_ID_Iter (Node_Annotations);
      
      while More (Iter) loop
         Next (Iter, A_Node);         
         Graph_Lib.Node_Id_Lists.Attach (The_List, A_Node);             
      end loop;
      
      return The_List;
   end Get_All_Annotated_Nodes;


   ---------------------------------------------------------------------------
   --  C
   --  Node Annotations - Write Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Add_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      Annotation       : in String) is
   begin

      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
      
      if (Is_Annotated (Node_Annotations, Node) = True) then
         raise Node_Is_Already_Annoated_Exception;
      end if;
      
      Node_Annotation_Hashed_Mappings.Bind
        (Node_Annotations.Annotations,
         Node,
         Ada.Strings.Unbounded.To_Unbounded_String (Annotation));
   end Add_Node_Annotation;

   ---------------------------------------------------------------------------
   procedure Change_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      New_Annotation   : in String) is
   begin
   
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
      
      if (Is_Annotated (Node_Annotations, Node) = False) then
         raise Node_Is_Not_Annotated_Exception;
      end if;     
      
      -- Remove existing entry
      Node_Annotation_Hashed_Mappings.Unbind 
        (Node_Annotations.Annotations, Node);
      
      -- Add new entry
      Node_Annotation_Hashed_Mappings.Bind
        (Node_Annotations.Annotations,
         Node,
         Ada.Strings.Unbounded.To_Unbounded_String (New_Annotation));     
   end Change_Node_Annotation;

   ---------------------------------------------------------------------------
   procedure Remove_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id) is
   begin
   
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
      
      if (Is_Annotated (Node_Annotations, Node) = False) then
         raise Node_Is_Not_Annotated_Exception;
      end if;   
      
      -- Remove existing entry
      Node_Annotation_Hashed_Mappings.Unbind 
        (Node_Annotations.Annotations, Node);       
   end Remove_Node_Annotation;
   
   
   ---------------------------------------------------------------------------
   --  D 
   --  Iterators
   ---------------------------------------------------------------------------
   
   ---------------------------------------------------------------------------
   function Make_Node_ID_Iter
     (Node_Annotations : in Node_Annotation_Access)
     return Node_ID_Iter is
     
   begin
      
      if Node_Annotations = null then
         raise Node_Annotation_Access_Not_Initialized_Exception;
      end if; 
         
      return Node_ID_Iter 
        (Node_Annotation_Hashed_Mappings.Make_Keys_Iter
          (Node_Annotations.Annotations));
   end Make_Node_ID_Iter;
   
   --------------------------------------------------------------------------- 
   function More (Iter : in Node_ID_Iter) return Boolean is
   
   begin
   
      return Node_Annotation_Hashed_Mappings.More
        (Node_Annotation_Hashed_Mappings.Keys_Iter (Iter));
   end More;
   
   ---------------------------------------------------------------------------
   procedure Next
     (Iter : in out Node_ID_Iter;
      Node :    out Graph_Lib.Node_Id) is
   begin
         
       Node_Annotation_Hashed_Mappings.Next
         (Node_Annotation_Hashed_Mappings.Keys_Iter (Iter),
          Node);       
   end next;
   
end Giant.Node_Annotations;




















