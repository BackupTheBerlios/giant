------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-config-class_sets.adb,v $, $Revision: 1.3 $
-- $Author: schwiemn $
-- $Date: 2003/06/17 14:03:02 $
--
with Giant.File_Management;  -- from GIANT
with Giant.XML_File_Access;  -- from GIANT
with Giant.Edge_Class_Proc;  -- from GIANT

with Unbounded_String_Hash; -- from Bauhaus IML "Reuse.src"

with Tree_Readers;       -- from xmlada

with DOM.Core;           -- from xmlada
with DOM.Core.Documents; -- from xmlada
with DOM.Core.Nodes;     -- from xmlada
with DOM.Core.Elements;  -- from xmlada

package body Giant.Config.Class_Sets is

   ---------------------------------------------------------------------------
   -- 0.1
   -- The internal data structure of the ADO
   ---------------------------------------------------------------------------

   -- used to organize all known class sets
   package Class_Sets_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Hash       => Unbounded_String_Hash,
      Value_Type => Class_Set_Access);

   -- holds all known class sets - ADO not initialized at this point
   -- Do not call "Class_Sets_Hashed_Mappings.Create" at this point.
   Class_Sets_Map : Class_Sets_Hashed_Mappings.Mapping;
   ADO_Initialized : Boolean := False;


   ---------------------------------------------------------------------------
   -- 0.2
   -- Internal subprograms
   ---------------------------------------------------------------------------

   -- deallocates a whole class set entry
   procedure Deallocate_Class_Set (Class_Set : in out Class_Set_Access) is

     procedure Free_Class_Set_Access is new Ada.Unchecked_Deallocation
     (Class_Set_Data, Class_Set_Access);

   begin
      -- deep deallocation
      Node_Class_Look_Up_Hashed_Mappings.Destroy (Class_Set.Node_Classes);
      Edge_Class_Look_Up_Hashed_Mappings.Destroy (Class_Set.Edge_Classes);

      -- deallocate class set itself
      Free_Class_Set_Access (Class_Set);
   end Deallocate_Class_Set;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and deallocation of the internal datastructure
   -- that holds all known class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Side Effect - Changed global variables:
   --   - Class_Sets_Map;
   procedure Initialize_Class_Sets 
     (GIANT_Class_Sets_Directory : in String) is

      ------------------------------------------------------------------------
      -- insert all edge classes from a edge class id set into the class set
      procedure Insert_Edge_Class_Id_Set_Into_Class_Set
        (The_Class_Set_Access : in Class_Set_Access;
         Edge_Class_Id_Set : in Graph_Lib.Edge_Class_Id_Set) is

         -- insert the element into the class set if it not already
         -- exists
         procedure Process_Element (Item : in Graph_Lib.Edge_Class_Id) is

         begin

            if not Edge_Class_Look_Up_Hashed_Mappings.Is_Bound
              (The_Class_Set_Access.Edge_Classes, Item) then

               Edge_Class_Look_Up_Hashed_Mappings.Bind
                 (The_Class_Set_Access.Edge_Classes, Item, Item);
            end if;
         end Process_Element;

         procedure Add_All_Elements_To_Class_Set is new
           Graph_Lib.Edge_Class_Id_Sets.Apply
           (Execute => Process_Element);

      begin

         Add_All_Elements_To_Class_Set (Edge_Class_Id_Set);
      end Insert_Edge_Class_Id_Set_Into_Class_Set;


      -- needed to insert new class sets
      New_Class_Set_Access : Class_Set_Access := null;

      -- needed for deallocation while replacing
      -- an old class set with a new (later read) one
      Remove_Class_Set_Access : Class_Set_Access := null;

      -- Needed for deallocation
      The_Tree_Reader  : Tree_Readers.Tree_Reader;
      -- top level node (document node)
      The_XML_Document : Dom.Core.Document;

      XML_Nodes_List : DOM.Core.Node_List;
      XML_Node : DOM.Core.Node;

      A_Node_Class_ID : Graph_Lib.Node_Class_Id;

      A_Edge_Class_ID_Set : Graph_Lib.Edge_Class_Id_Set;

      File_List : String_Lists.List;
      File_List_Iter : String_Lists.ListIter;
      A_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      
      -- needed to determine which files should be ignored
      Ignore_File : Boolean := False;

   begin

      -- Get all xml files in passed dir
      begin
         File_List := File_Management.Get_Filtered_Files_From_Directory
           (GIANT_Class_Sets_Directory, True, ".xml");
      exception
         when File_Management.Invalid_Directory_Exception =>
            raise Invalid_Class_Set_Directory_Exception;
      end;
      -- Now we have all absolute paths for the files holding
      -- the class sets

      -- Load each class set into internal datastructure
      File_List_Iter := String_Lists.MakeListIter (File_List);

      -- INITIALISATION
      -- Create new empty top level hash map holding all class sets and
      -- initializes the ADO
      Class_Sets_Map := Class_Sets_Hashed_Mappings.Create;

      -- Begin: Processing all xml files for class sets
      while String_Lists.More (File_List_Iter) loop

         -- Think positive - process files until security check says 
         -- something else
         Ignore_File := False;

         -- get a file to process
         String_Lists.Next (File_List_Iter, A_File_Name);

         -- ignore not readable xml files
         begin
            XML_File_Access.Load_XML_File_Validated
              (Ada.Strings.Unbounded.To_String (A_File_Name),
               The_Tree_Reader,
               The_XML_Document);
               
         exception
            when XML_File_Access.XML_File_Access_Error_Exception =>
               Ignore_File := True;   
            when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
               Ignore_File := True;   
         end;
         
         -- ignore files that do not describe a class set
         if ( (Ignore_File = False) and then
            (XML_File_Access.Does_XML_Document_Belong_To_Type
              ("giant_class_set_file", The_XML_Document) = False) ) then

            Tree_Readers.Free (The_Tree_Reader);
            Ignore_File := True; 
         end if;
                  
         if (Ignore_File = False) then

            -- create a new class set with existing data
            ---------------------------------------------
            New_Class_Set_Access := new Class_Set_Data;
            New_Class_Set_Access.Node_Classes :=
              Node_Class_Look_Up_Hashed_Mappings.Create;
            New_Class_Set_Access.Edge_Classes :=
              Edge_Class_Look_Up_Hashed_Mappings.Create;
     
            -- get name of class_set (the file name without ending and path)
            -- file name: "\class_sets\my_set.xml" -->
            -- name of class set: "my_set";
            -- an already existing class set with the same name will be 
            --  replaced.
            New_Class_Set_Access.Class_Set_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (File_Management.Calculate_Name_For_File
                   (Ada.Strings.Unbounded.To_String (A_File_Name)));
         end if;   

         if (Ignore_File = False) then

            -- process node classes
            ------------------------------------------------------------------
            -- Get_All_Node_Classes
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "node_class");

            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               -- IGNORE node classes not known by the IML Reflection
               if Graph_Lib.Does_Node_Class_Exist
                 (DOM.Core.Elements.Get_Attribute
                  (XML_Node, "node_class_name")) then
              
                  -- Calculate node class ID
                  A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
                    (DOM.Core.Elements.Get_Attribute
                     (XML_Node, "node_class_name"));

                 -- Check whether the node class is already in the set
                 if not Node_Class_Look_Up_Hashed_Mappings.Is_Bound
                   (New_Class_Set_Access.Node_Classes, A_Node_Class_ID) then

                    -- Add node class to Class set
                    Node_Class_Look_Up_Hashed_Mappings.Bind
                      (New_Class_Set_Access.Node_Classes,
                       A_Node_Class_ID,
                       A_Node_Class_ID);

                  end if;
               end if;
            end loop;            

            DOM.Core.Free (XML_Nodes_List);

            -- process edge classes
            ------------------------------------------------------------------
            -- get all entries for edge classes
            XML_Nodes_List :=
              DOM.Core.Documents.Get_Elements_By_Tag_Name
              (The_XML_Document, "edge_class");

            -- process all entries
            for I in 0 .. DOM.Core.Nodes.Length (XML_Nodes_List) - 1 loop

               XML_Node := DOM.Core.Nodes.Item (XML_Nodes_List, I);

               -- calculate set of edge classes specified by this node
               A_Edge_Class_ID_Set := 
                 Edge_Class_Proc.Process_Edge_Class_Entry (XML_Node);

               -- check whether returned set is empty
               if Graph_Lib.Edge_Class_Id_Sets.Is_Empty
                 (A_Edge_Class_ID_Set) then

                  Graph_Lib.Edge_Class_Id_Sets.Destroy 
                    (A_Edge_Class_ID_Set);
              
               else

                  -- insert all edge class id's from the calculated set into
                  -- the class set
                  Insert_Edge_Class_Id_Set_Into_Class_Set
                    (New_Class_Set_Access, A_Edge_Class_ID_Set);

                  -- deallocate returned edge class id set
                  Graph_Lib.Edge_Class_Id_Sets.Destroy 
                    (A_Edge_Class_ID_Set);
                 
               end if;    
                
            end loop;

            DOM.Core.Free (XML_Nodes_List);

            -- Free memory needed for the xml dom tree
            Tree_Readers.Free(The_Tree_Reader);

            -- Insert the complete new class set into the ADO's data structure
            ------------------------------------------------------------------

            -- Check whether the new Class_Set_Name already exists and 
            -- remove the old one then necessary.
            if (Class_Sets_Hashed_Mappings.Is_Bound
                (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name)) then

               -- get already inserted class set with same name
               Remove_Class_Set_Access :=
                 Class_Sets_Hashed_Mappings.Fetch
                   (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name);

               -- remove old class set from ADO
               Class_Sets_Hashed_Mappings.Unbind
                 (Class_Sets_Map, New_Class_Set_Access.Class_Set_Name);

               -- deallocate old class set
               Deallocate_Class_Set (Remove_Class_Set_Access);

            end if;

               -- insert new class set
               Class_Sets_Hashed_Mappings.Bind
                 (Class_Sets_Map,
                  New_Class_Set_Access.Class_Set_Name,
                  New_Class_Set_Access);

         end if; -- if (Ignore_File = False) 
         
      end loop; -- End: Processing all xml files for class sets

      -- Deallocate File_List
      String_Lists.Destroy (File_List);

      ADO_Initialized := True;
   end Initialize_Class_Sets;

   ---------------------------------------------------------------------------
   procedure Clear_Class_Sets is

      -- needed for deeop deallocation
      Class_Sets_Iter : Class_Sets_Hashed_Mappings.Values_Iter;
      Dealloc_Class_Set_Access : Class_Set_Access;
   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      Class_Sets_Iter := Class_Sets_Hashed_Mappings.Make_Values_Iter
        (Class_Sets_Map);

      -- deep deallocation for each class set
      while Class_Sets_Hashed_Mappings.More (Class_Sets_Iter) loop
         Class_Sets_Hashed_Mappings.Next
           (Class_Sets_Iter,
            Dealloc_Class_Set_Access);

         Deallocate_Class_Set (Dealloc_Class_Set_Access);
      end loop;

      -- deallocate top level hash map needed to manage all class sets
      Class_Sets_Hashed_Mappings.Destroy (Class_Sets_Map);

      -- mark ADO as not initialized
      ADO_Initialized := False;

   end Clear_Class_Sets;


   ---------------------------------------------------------------------------
   -- B
   -- Access to class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_All_Existing_Class_Sets
     return String_Lists.List is

      Names_List : String_Lists.List;
      -- Iterate over all known class sets;
      Class_Sets_Iter : Class_Sets_Hashed_Mappings.Values_Iter;

      A_Class_Set_Access : Class_Set_Access;

   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      Class_Sets_Iter :=
        Class_Sets_Hashed_Mappings.Make_Values_Iter (Class_Sets_Map);
      Names_List := String_Lists.Create;

      while Class_Sets_Hashed_Mappings.More (Class_Sets_Iter) loop
         Class_Sets_Hashed_Mappings.Next
           (Class_Sets_Iter,
            A_Class_Set_Access);
         String_Lists.Attach (Names_List, A_Class_Set_Access.Class_Set_Name);
      end loop;

      return Names_List;
   end Get_All_Existing_Class_Sets;


   ---------------------------------------------------------------------------
   function Does_Class_Set_Exist
     (Class_Set_Name : in String)
     return Boolean is

   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      return Class_Sets_Hashed_Mappings.Is_Bound
        (Class_Sets_Map,
         Ada.Strings.Unbounded.To_Unbounded_String(Class_Set_Name));
   end Does_Class_Set_Exist;

   ---------------------------------------------------------------------------
   function Initialize_Class_Set_Access
     (Class_Set_Name : in String)
     return Class_Set_Access is

   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Does_Class_Set_Exist (Class_Set_Name) = False) then
         raise Class_Set_Does_Not_Exist_Exception;
      end if;

      return Class_Sets_Hashed_Mappings.Fetch
        (Class_Sets_Map,
         Ada.Strings.Unbounded.To_Unbounded_String(Class_Set_Name));
   end Initialize_Class_Set_Access;


   ---------------------------------------------------------------------------
   -- D
   -- Queries to class sets.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Is_Node_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Boolean is

   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Class_Set = null) then
         raise Class_Sets_Access_Not_Initialized_Exception;
      end if;

      return
        Node_Class_Look_Up_Hashed_Mappings.Is_Bound
        (Class_Set.Node_Classes, Node_Class);
   end Is_Node_Class_Element_Of_Class_Set;

   ---------------------------------------------------------------------------
   function Is_Edge_Class_Element_Of_Class_Set
     (Class_Set  : in Class_Set_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Boolean is

   begin

      if (ADO_Initialized = False) then
         raise Class_Sets_ADO_Not_Initialized_Exception;
      end if;

      if (Class_Set = null) then
         raise Class_Sets_Access_Not_Initialized_Exception;
      end if;

      return
        Edge_Class_Look_Up_Hashed_Mappings.Is_Bound
        (Class_Set.Edge_Classes, Edge_Class);
   end Is_Edge_Class_Element_Of_Class_Set;
end Giant.Config.Class_Sets;
