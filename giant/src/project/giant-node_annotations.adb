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
--  $RCSfile: giant-node_annotations.adb,v $, $Revision: 1.2 $
--  $Author: schwiemn $
--  $Date: 2003/05/27 09:57:08 $
--
package body Giant.Node_Annotations is


   ---------------------------------------------------------------------------
   -- 0.1
   -- Internal subprograms
   ---------------------------------------------------------------------------
     
      
   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, persistence and deallocation.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Load_From_File
     (Node_Annotations_File : in String)
      return Node_Annotation_Access is
      
      New_Node_Annotation_Access : Node_Annotation_Access := null;   
      
      Annotation_Tree_Reader : ree_Readers.Tree_Reader;
      Annotation_XML_Document : Dom.Core.Document;
      
      XML_Annotation_Nodes_List : DOM.Core.Node_List;      
      XML_Annotation_Node : DOM.Core.Node;
      XML_Annotation_Text_Node : DOM.Core.Node; 
                             
   begin
  
      --  access and check the xml file
      --------------------------------
      begin
   
         XML_File_Access.Load_XML_File_Validated
          (Ada.Strings.Unbounded.To_String (A_File_Name),
           Annotation_Tree_Reader,
            Annotation_XML_Document);
               
      exception
         when XML_File_Access.XML_File_Access_Error_Exception =>
            raise Node_Annotations_File_Not_Found_Exception;

         when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
            raise Node_Annotations_File_Not_Correct_Exception;
      end;
         
      if ( XML_File_Access.Does_XML_Document_Belong_To_Type
        ("giant_node_annotations_file", The_XML_Document) = False) ) then
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
         !!!!!!!!!!!!!!!!!!!!!
         
         
           
         
               
               
               
               
               
               
               

      -- deallocate resources
      DOM.Core.Free (XML_Nodes_List);
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
   procedure Add_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      Annotation       : in String) is
   begin
      null;
   end Add_Node_Annotation;

   ---------------------------------------------------------------------------
   procedure Change_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id;
      New_Annotation   : in String) is
   begin
      null;
   end Change_Node_Annotation;

   ------------------
   -- Create_Empty --
   ------------------


   ---------------------------------------------------------------------------
   procedure Deallocate_Node_Annotation_Access
     (Node_Annotations : in out Node_Annotation_Access) is
   begin
      null;
   end Deallocate_Node_Annotation_Access;

   ---------------------------------------------------------------------------
   function Get_Annotation_Text
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
      return String is
   begin
      return Get_Annotation_Text (Node_Annotations, Node);
   end Get_Annotation_Text;

   ---------------------------------------------------------------------------
   function Is_Annotated
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id)
      return Boolean is
   begin
      return Is_Annotated (Node_Annotations, Node);
   end Is_Annotated;


   ---------------------------------------------------------------------------
   procedure Remove_Node_Annotation
     (Node_Annotations : in Node_Annotation_Access;
      Node             : in Graph_Lib.Node_Id) is
   begin
      null;
   end Remove_Node_Annotation;

   ---------------------------------------------------------------------------
   procedure Write_To_File
     (Node_Annotations : in Node_Annotation_Access;
      Node_Annotations_File : in String) is
   begin
      null;
   end Write_To_File;

end Giant.Node_Annotations;

