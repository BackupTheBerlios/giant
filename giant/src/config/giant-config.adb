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
-- $RCSfile: giant-config.adb,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 07:50:13 $
--
with Ada.Unchecked_Deallocation;

with String_Lists; -- from Bauhaus IML "Reuse.src"
with Ordered_Sets; -- from Bauhaus IML "Reuse.src"

with Giant.Ptr_Normal_Hashs; -- from GIANT
with Giant.XML_File_Access;  -- from GIANT
with Giant.XPM_File_Access;  -- from GIANT
with Giant.File_Management;  -- from GIANT
with Giant.Graph_Lib;        -- from GIANT

with Input_Sources.File; -- from xmlada
with Tree_Readers;       -- from xmlada
with Sax.Readers;        -- from xmlada

with DOM.Core.Nodes;     -- from xmlada
with DOM.Core.Documents; -- from xmlada
with DOM.Core.Elements;  -- from xmlada

package body Giant.Config is


   ---------------------------------------------------------------------------
   -- 0.1
   -- The internal data structure of the ADO
   ---------------------------------------------------------------------------

   -- determines the status of the ADO - initialized or false
   ADO_Initialized : Boolean := False;

   ---------------------------------------------------------------------------
   -- Describes one user defined setting
   type User_Def_Set_Entry is record
      -- must be unique for each setting
      Unique_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Setting_Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   -- WARNING: Changing this function will seriously affect the
   -- hole package
   function User_Def_Set_Entry_Equal
     (Left : User_Def_Set_Entry; Right : User_Def_Set_Entry)
     return Boolean is
   begin

      return  Ada.Strings.Unbounded."="
        (Left.Unique_Name, Right.Unique_Name);
   end User_Def_Set_Entry_Equal;

   -- WARNING: Changing this function will seriously affect the
   -- hole package
   function User_Def_Set_Entry_Less_Than
     (Left : User_Def_Set_Entry; Right : User_Def_Set_Entry)
     return Boolean is

   begin
      return  Ada.Strings.Unbounded."<"
        (Left.Unique_Name, Right.Unique_Name);
   end User_Def_Set_Entry_Less_Than;

   -- administartion of free defined settings
   package Setting_Data_Sets is new Ordered_Sets
     (Item_Type => User_Def_Set_Entry,
      "="       => User_Def_Set_Entry_Equal,
      "<"       => User_Def_Set_Entry_Less_Than);

   ----- needed to organize Highlight_Colors
   type Selection_High_Light_Colors_Array is array
     (Selection_High_Light_ID'Range) of Color_Access;

   type IML_Subgraph_High_Light_Colors_Array is array
     (IML_Subgraph_High_Light_ID'Range) of Color_Access;


   ---------------------------------------------------------------------------
   -- Internal global variables

   -- Holds all config settings;
   ADO_Free_Defined_Settings_Set : Setting_Data_Sets.Set :=
     Setting_Data_Sets.Empty_Set;


   -- Holds the icon used to show that a window node
   -- is annotated.
   Node_Annotation_Icon : Chars_Ptr_Array_Access;


   -- Highlight colors for selections
   Actual_Selection_Highlight_Color : Color_Access := null;

   Selection_Highlight_Colors :
     Selection_High_Light_Colors_Array := (others => null);

   -- Highlight colors for IML_Subgraphs
   IML_Subgraph_High_Light_Colors :
     IML_Subgraph_High_Light_Colors_Array := (others => null);


   ---------------------------------------------------------------------------
   -- 0.2
   -- Internal utility subprograms.
   -------------------------------------

   -------------------------------------
   -- check whether ADO is initialized
   function Is_Config_ADO_Initialized return Boolean is
   begin

      if (ADO_Initialized = True) then

         return True;
      end if;

      return False;
   end Is_Config_ADO_Initialized;

   ---------------------------------------------------------------------------
   -- Needed by subpackages for the processing of edge class entries in
   -- xml files.
   -- Passed node must have the following attributes specified in DTD:
   --
   -- May return an empty set.
   --
   -- <!ATTLIST edge_class
   --   start_node_class CDATA #REQUIRED
   --   attribute_name   CDATA #REQUIRED
   -- >
   --
   function Process_Edge_Class_Entry (XML_Node : in DOM.Core.Node) return
     Graph_Lib.Edge_Class_Id_Set is

      New_Edge_Class_Set : Graph_Lib.Edge_Class_Id_Set :=
        Graph_Lib.Edge_Class_Id_Set_Package.Empty_Set;

      A_Node_Class_ID : Graph_Lib.Node_Class_Id;
      A_Node_Attribute_ID : Graph_Lib.Node_Attribute_Id;

      A_Edge_Class_ID : Graph_Lib.Edge_Class_Id;
   begin

      -- Case 1 - one single edge class specified by the xml node
      if (DOM.Core.Elements.Get_Attribute
          (XML_Node, "start_node_class") /="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") /="*") then

         -- check whether edge class and node class name
         -- do exist
         if Graph_Lib.Does_Node_Class_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"))
           and Graph_Lib.Does_Node_Attribute_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"),
            DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name")) then

            A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
              (DOM.Core.Elements.Get_Attribute
                (XML_Node, "start_node_class"));

            A_Node_Attribute_ID := Graph_Lib.Convert_Node_Attribute_Name_To_Id
              (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"),
               DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name"));


            -- Check whether this attribute is an edge
            if Graph_Lib.Does_Edge_Class_Exist
              (A_Node_Class_ID, A_Node_Attribute_ID) then

               A_Edge_Class_ID :=
                 Graph_Lib.Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
                 (A_Node_Class_ID, A_Node_Attribute_ID);

               Graph_Lib.Edge_Class_Id_Set_Package.Insert
                 (New_Edge_Class_Set, A_Edge_Class_ID);

               -- Holds one edge class id
               return New_Edge_Class_Set;
            end if;
         end if;

         -- Case 2 - one node class, vildcard for attributes
      elsif (DOM.Core.Elements.Get_Attribute
             (XML_Node, "start_node_class") /="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") ="*") then

         if Graph_Lib.Does_Node_Class_Exist
           (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"))
         then

            A_Node_Class_ID := Graph_Lib.Convert_Node_Class_Name_To_Id
              (DOM.Core.Elements.Get_Attribute (XML_Node, "start_node_class"));

            return Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Class
              (A_Node_Class_ID);
         end if;

         -- Case 3 - vildcard for node classes, one single attribute.
      elsif (DOM.Core.Elements.Get_Attribute
             (XML_Node, "start_node_class") ="*") and
        (DOM.Core.Elements.Get_Attribute
         (XML_Node, "attribute_name") /="*") then

            return Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Attribute
              (DOM.Core.Elements.Get_Attribute (XML_Node, "attribute_name"));
      end if;

      -- Return an empty set
      -- (statement only reachable if an empty set is returned).
      return New_Edge_Class_Set;
   end Process_Edge_Class_Entry;

   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Reads all config data out of a xml file of
   -- type "giant_global_config_file"
   --
   -- SIDE-EFFECTS - Changes global variables (forming the ADO):
   --   - ADO_Free_Defined_Settings_Set
   --   - Node_Annotation_Icon
   --   - Actual_Selection_Highlight_Color
   --   - Selection_Highlight_Colors
   --   - IML_Subgraph_High_Light_Colors
   --
   procedure Initialize_Config_Data
     (GIANT_Config_File : in String;
      User_Config_File  : in String) is

      ------------------------------------------------------------------------
      -- Overites already existing entries
      -- used to read the global config file
      --
      -- may only be used for documents of type "giant_global_config_file"
      procedure Read_Config_Entries
        (Source_XML_Document : in Dom.Core.Document;
         The_Config_File : in String) is

         -- holds all nodes descibing a user defined setting
         Setting_Nodes_List : DOM.Core.Node_List;

         -- a single node holding a user defined setting
         A_Setting_Node : DOM.Core.Node;

         -- a record holding the data describing a setting
         A_Parsed_Setting : User_Def_Set_Entry;

         -- Used to expand relative paths to absolute paths
         Config_File_Dir_Path : String :=
           File_Management.Return_Dir_Path_For_File_Path (The_Config_File);

      begin

         -----------------------------------
         -- get settings that do not describe paths and files
         Setting_Nodes_List :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name
           (Source_XML_Document, "a_setting");

         -- insert new setting with "unique_name" a possibly already
         -- existing setting with this name will be removed.
         for I in 0 ..  DOM.Core.Nodes.Length(Setting_Nodes_List) - 1 loop

            A_Setting_Node := DOM.Core.Nodes.Item(Setting_Nodes_List,I);

            A_Parsed_Setting.Unique_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (DOM.Core.Elements.Get_Attribute
               (A_Setting_Node, "setting_name"));

            A_Parsed_Setting.Setting_Value :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (DOM.Core.Elements.Get_Attribute
               (A_Setting_Node, "value"));

            -- check if setting already exists and remove it if that
            -- is the case - it will later be replaced by a setting
            -- with the same Unique_Name but possibly another
            -- Setting_Value.
            if Setting_Data_Sets.Is_Member
              (A_Set   => ADO_Free_Defined_Settings_Set,
               Element => A_Parsed_Setting) then

               -- only possible as Elements are differed by
               -- "User_Def_Set_Entry.Unique_Name"
               Setting_Data_Sets.Remove
                 (A_Set   => ADO_Free_Defined_Settings_Set,
                  Element => A_Parsed_Setting);

            end if;

            Setting_Data_Sets.Insert
              (A_Set   => ADO_Free_Defined_Settings_Set,
               Element => A_Parsed_Setting);

         end loop;

         -- deallocate used memory for list
         DOM.Core.Free (Setting_Nodes_List);

         ---------------------
         -- Process all paths to files
         -- relative paths are changed to absolute paths

         Setting_Nodes_List :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name
           (Source_XML_Document, "a_path_setting");

         -- insert new setting with "unique_name" a possibly already
         -- existing setting with this name will be removed.
         for I in 0 ..  DOM.Core.Nodes.Length(Setting_Nodes_List) - 1 loop

            A_Setting_Node := DOM.Core.Nodes.Item(Setting_Nodes_List,I);

            A_Parsed_Setting.Unique_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (DOM.Core.Elements.Get_Attribute
               (A_Setting_Node, "setting_name"));


            -- expand relative Paths if "Value" is an absolute path
            -- then it will not be changed.
            A_Parsed_Setting.Setting_Value :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Get_Absolute_Path_To_File_From_Relative
               (Config_File_Dir_Path,
                DOM.Core.Elements.Get_Attribute
                (A_Setting_Node, "value")
                )
               );

                 -- check if setting already exists and remove it if that
                 -- is the case - it will later be replaced by a setting
                 -- with the same Unique_Name but possibly another
                 -- Setting_Value.
                 if Setting_Data_Sets.Is_Member
                 (A_Set   => ADO_Free_Defined_Settings_Set,
                 Element => A_Parsed_Setting) then

            -- only possible as Elements are differed by
            -- "User_Def_Set_Entry.Unique_Name"
            Setting_Data_Sets.Remove
            (A_Set   => ADO_Free_Defined_Settings_Set,
             Element => A_Parsed_Setting);

                 end if;

                 Setting_Data_Sets.Insert
                   (A_Set   => ADO_Free_Defined_Settings_Set,
                    Element => A_Parsed_Setting);

         end loop;

         -- deallocate used memory for list
         DOM.Core.Free (Setting_Nodes_List);


      end Read_Config_Entries;

      ------------------------------------------------------------------------
      -- Used to avoid clones as this is done for GIANT_Config_File
      -- and User_Config_File
      procedure Process_Config_File (Config_File : in String) is

         -- Needed for deallocation
         The_Tree_Reader  : Tree_Readers.Tree_Reader;

         -- top level node (document node)
         The_XML_Document : Dom.Core.Document;

         Node_Anno_Pic_File : Ada.Strings.Unbounded.Unbounded_String;

      begin

         -- Load and Parse Config_File
         begin
            XML_File_Access.Load_XML_File_Validated
              (Config_File, The_Tree_Reader, The_XML_Document);
         exception
            when XML_File_Access.XML_File_Access_Error_Exception =>
               raise Config_File_Could_Not_Be_Accessed_Exception;
            when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>
               raise Config_File_Not_Correct_Exception;
         end;

         -- check for correct type
         if (XML_File_Access.Does_XML_Document_Belong_To_Type
             ("giant_global_config_file", The_XML_Document) = False) then

            raise Config_File_Not_Correct_Exception;
         end if;

         -- read data into internal data structures
         begin
            -- processes all <a_setting> and <a_path_setting> nodes
            Read_Config_Entries(The_XML_Document, Config_File);

            -- removes the complete xml document "The_XML_Document"
            -- from memory
            Tree_Readers.Free(The_Tree_Reader);

         exception
            when others =>
               Tree_Readers.Free(The_Tree_Reader);
               raise Config_File_Not_Correct_Exception;
         end;

      end Process_Config_File;
      ------------------------------------------------------------------------
   begin

      -------------
      -- Thist step
      -- Read all settings from GIANT_Config_File
      Process_Config_File (GIANT_Config_File);

      --------------
      -- Second step
      -- Read all settings from optional User_Config_File
      -- Settings defined User_Config_File that were already read from
      -- GIANT_Config_File will be replaced.
      if not (User_Config_File = "") then

         Process_Config_File (User_Config_File);
      end if;

      -- Further processing of some special settings
      -- Check whether all required settings exists
      if not Does_Setting_Exist ("Icon_For_Node_Annotations")
        or not Does_Setting_Exist ("Actual_Selection_Highlight_Color")
        or not Does_Setting_Exist ("Selection_Highlight_Color_1")
        or not Does_Setting_Exist ("Selection_Highlight_Color_2")
        or not Does_Setting_Exist ("Selection_Highlight_Color_3")
        or not Does_Setting_Exist ("IML_Subgraph_Highlight_Color_1")
        or not Does_Setting_Exist ("IML_Subgraph_Highlight_Color_2")
        or not Does_Setting_Exist ("IML_Subgraph_Highlight_Color_2") then

         raise Config_File_Not_Correct_Exception;
      end if;

      -- organize color pointers
      -- selections
      Actual_Selection_Highlight_Color :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("Actual_Selection_Highlight_Color"));

      Selection_Highlight_Colors (Color_1) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("Selection_Highlight_Color_1"));

      Selection_Highlight_Colors (Color_2) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("Selection_Highlight_Color_2"));

      Selection_Highlight_Colors (Color_3) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("Selection_Highlight_Color_3"));

      -- iml subgraphs
      IML_Subgraph_High_Light_Colors (Color_1) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("IML_Subgraph_Highlight_Color_1"));

      IML_Subgraph_High_Light_Colors (Color_2) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("IML_Subgraph_Highlight_Color_2"));

      IML_Subgraph_High_Light_Colors (Color_3) :=
        new Ada.Strings.Unbounded.Unbounded_String'
        (Return_Setting_As_String ("IML_Subgraph_Highlight_Color_3"));

      -- load node annotation icon into main memory
      begin

         Node_Annotation_Icon :=
           new Gtkada.Types.Chars_Ptr_Array'
           (XPM_File_Access.Read_Pixmap_File
            (Ada.Strings.Unbounded.To_String
             (Return_Setting_As_String
              ("Icon_For_Node_Annotations"))) );

      exception
         when others =>
            raise Config_File_Not_Correct_Exception;

      end;

      -- Mark ADO as initialized
      ADO_Initialized := True;
   end Initialize_Config_Data;

   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is

   begin

      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      -- Free set holding config settings
      Setting_Data_Sets.Destroy(ADO_Free_Defined_Settings_Set);

      -- Free memory used to store the node annotation icon
      Gtkada.Types.Free (Node_Annotation_Icon.all);

      -- Free pointer
      Free_Chars_Ptr_Array_Access (Node_Annotation_Icon);

      -- Free momory used to store color strings
      Free_Color_Access (Actual_Selection_Highlight_Color);

      For I in Selection_Highlight_Colors'Range loop

         Free_Color_Access (Selection_Highlight_Colors (I));
      end loop;

      For I in IML_Subgraph_High_Light_Colors'Range loop

         Free_Color_Access (IML_Subgraph_High_Light_Colors (I));
      end loop;

      -- Mark ADO as not initialized
      ADO_Initialized := False;
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- General Access to configuration data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Setting_Exist (Name_Of_Setting : in String)
                               return Boolean is

      A_Dummy_Setting : User_Def_Set_Entry;

   begin

      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      -- Create a dummy element that is used to
      -- determine whether a Element with Unique_Name = Name_Of_Setting
      -- exists in the set.
      A_Dummy_Setting.Unique_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String(Name_Of_Setting);
      A_Dummy_Setting.Setting_Value :=
        Ada.Strings.Unbounded.To_Unbounded_String("");

      if Setting_Data_Sets.Is_Member
        (A_Set   => ADO_Free_Defined_Settings_Set,
         Element => A_Dummy_Setting) then

         return true;
      end if;

      return  False;
   end Does_Setting_Exist;

   ---------------------------------------------------------------------------
   function Return_Setting_As_String
     (Name_Of_Setting : in String)
     return Ada.Strings.Unbounded.Unbounded_String is

      A_Dummy_Setting : User_Def_Set_Entry;

   begin

      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      if (Does_Setting_Exist(Name_Of_Setting) = False) then

         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      A_Dummy_Setting.Unique_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String(Name_Of_Setting);
      A_Dummy_Setting.Setting_Value :=
        Ada.Strings.Unbounded.To_Unbounded_String("");

      A_Dummy_Setting := Setting_Data_Sets.Get
        (ADO_Free_Defined_Settings_Set, A_Dummy_Setting);

      return A_Dummy_Setting.Setting_Value;
   end Return_Setting_As_String;

   ---------------------------------------------------------------------------
   function Return_Setting_As_Integer (Name_Of_Setting : in String)
                                      return Integer is

      String_Val : Ada.Strings.Unbounded.Unbounded_String;
      Int_Val : Integer;
   begin

      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      if (Does_Setting_Exist(Name_Of_Setting) = False) then

         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      String_Val := Return_Setting_As_String(Name_Of_Setting);

      begin
         Int_Val := Integer'Value
           (Ada.Strings.Unbounded.To_String (String_Val));
      exception
         when Constraint_Error =>
            raise Config_Setting_Is_No_Integer_Value_Exception;
      end;

      return Int_Val;
   end Return_Setting_As_Integer;


   ---------------------------------------------------------------------------
   -- C
   -- Access to the configuration data about colors.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Return_Highlight_Color_For_Actual_Selection
     return Color_Access is
   begin
      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Actual_Selection_Highlight_Color;
   end Return_Highlight_Color_For_Actual_Selection;

   ---------------------------------------------------------------------------
   function Return_Highlight_Color_For_Selection
     (Highlight_ID : in Selection_High_Light_ID)
     return Color_Access is

   begin
      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Selection_Highlight_Colors (Highlight_ID);
   end Return_Highlight_Color_For_Selection;

   ---------------------------------------------------------------------------
   function Return_Highlight_Color_For_IML_Subgraph
     (Highlight_ID : in IML_Subgraph_High_Light_ID)
     return Color_Access is
   begin
      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return IML_Subgraph_High_Light_Colors (Highlight_ID);
   end Return_Highlight_Color_For_IML_Subgraph;

   ---------------------------------------------------------------------------
   function Return_Icon_For_Node_Annotations
     return Chars_Ptr_Array_Access is

   begin
      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Node_Annotation_Icon;
   end Return_Icon_For_Node_Annotations;


   ---------------------------------------------------------------------------
   -- D
   -- Color_Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Color_Value (Color_Ptr : in Color_Access) return String is
   begin

      if (Color_Ptr = null) then
         raise Color_Access_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String (Color_Ptr.all);
   end Get_Color_Value;

   ---------------------------------------------------------------------------
   package Hash_Color_Access_Hashs
   is new Ptr_Normal_Hashs
     (Ada.Strings.Unbounded.Unbounded_String,
      Color_Access);

   function Hash_Color_Access (Color_Ptr : in Color_Access) return Integer is
   begin

      if (Color_Ptr = null) then
         raise Color_Access_Not_Initialized_Exception;
      end if;

      return Hash_Color_Access_Hashs.Integer_Hash (Color_Ptr);
   end Hash_Color_Access;

end Giant.Config;
