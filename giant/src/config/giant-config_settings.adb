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
-- $RCSfile: giant-config_settings.adb,v $, $Revision: 1.4 $
-- $Author: schwiemn $
-- $Date: 2003/06/22 16:14:50 $
--
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with Hashed_Mappings;       -- from Bauhaus Reuse.src
pragma Elaborate_All (Hashed_Mappings);
with Unbounded_String_Hash; -- from Bauhaus Reuse.src

with Tree_Readers;       -- from xmlada
with DOM.Core;           -- from xmlada
with DOM.Core.Nodes;     -- from xmlada
with DOM.Core.Documents; -- from xmlada
with DOM.Core.Elements;  -- from xmlada

with Giant.Logger;          -- from GIANT
with Giant.XML_File_Access; -- from GIANT

package body Giant.Config_Settings is

   -- logging functionality
   package Logger is new Giant.Logger ("Giant.Config_Settings");

   --------------------------------------------------------------------------
   --  Internal Datastructure of the ADO
   --------------------------------------------------------------------------

   package Setting_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Ada.Strings.Unbounded.Unbounded_String);

   -- Settings read from "GIANT_Config_File"
   Giant_Settings_Map : Setting_Hashs.Mapping;
   -- the root for relative paths - needed for expansion
   Giant_Config_File_Abs_Path_Root : Ada.Strings.Unbounded.Unbounded_String;
   -- the directory there the config file is located - needed for expansion
   Giant_Config_File_Dir : Ada.Strings.Unbounded.Unbounded_String;

   -- Settings read from "User_Config_File"
   User_Settings_Map  : Setting_Hashs.Mapping;
   User_Config_File_Abs_Path_Root : Ada.Strings.Unbounded.Unbounded_String;
   -- the directory there the config file is located - needed for expansion
   User_Config_File_Dir : Ada.Strings.Unbounded.Unbounded_String;

   -- Used to check initialisation
   ADO_Initialized : Boolean := False;

   --------------------------------------------------------------------------
   -- 0.1 Validators
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Validate_Integer (Value : in String) return Boolean is
     Test_Int : Integer;
   begin
      begin
        Test_Int := Integer'Value (Value);
      exception
         when Constraint_Error =>

            return False;
      end;

      return True;
   end Validate_Integer;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Adds defaults values for settings - if no entry is found in both Maps
   procedure Add_Default_Values_If_Necessary is
   begin
      for I in Default_Settings'Range loop
         if not Does_Setting_Exist
           (Ada.Strings.Unbounded.To_String (Default_Settings (I).Name))
           then

            -- add entry
            Setting_Hashs.Bind
              (Giant_Settings_Map,
               Default_Settings (I).Name,
               Default_Settings (I).Def_Value);
         end if;
      end loop;
   end Add_Default_Values_If_Necessary;

   ---------------------------------------------------------------------------
   procedure Validate_Read_Settings
     (Source_Map : in out Setting_Hashs.Mapping) is

   begin

      for I in Default_Settings'Range loop

         if (Default_Settings (I).Validator_Function /= null) and then
           Setting_Hashs.Is_Bound (Source_Map, Default_Settings (I).Name) then

           -- replace with default if validation failes
           if not Default_Settings (I).Validator_Function
             (Ada.Strings.Unbounded.To_String
               (Setting_Hashs.Fetch (Source_Map, Default_Settings (I).Name)))
             then

              Logger.Info
                ("Validation failed for config setting with name: """
                 & Ada.Strings.Unbounded.To_String
                   (Default_Settings (I).Name)
                 & """ - value: """
                 & Ada.Strings.Unbounded.To_String
                     (Setting_Hashs.Fetch
                       (Source_Map, Default_Settings (I).Name))
                 & """ not valid");

              Setting_Hashs.Update_Value
                (Source_Map,
                 Default_Settings (I).Name,
                 Default_Settings (I).Def_Value);
           end if;
         end if;
      end loop;
   end Validate_Read_Settings;

   ---------------------------------------------------------------------------
   procedure Read_Setting_Entries_Into_Hash_Map
     (Source_XML_Document : in     Dom.Core.Document;
      Target_Map          : in out Setting_Hashs.Mapping) is

      -- holds all <setting> nodes descibing user defined settings
      Setting_Nodes_List : DOM.Core.Node_List;
      A_Setting_Node     : DOM.Core.Node;

      Setting_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Setting_Value      : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- get all <setting> nodes
      --------------------------
      Setting_Nodes_List :=
         DOM.Core.Documents.Get_Elements_By_Tag_Name
           (Source_XML_Document, "setting");

      -- insert new setting with "a possibly already
      -- existing setting with the same name will be replaced
      -------------------------------------------------------
      for I in 0 ..  DOM.Core.Nodes.Length(Setting_Nodes_List) - 1 loop

         A_Setting_Node := DOM.Core.Nodes.Item(Setting_Nodes_List,I);

         Setting_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
            (DOM.Core.Elements.Get_Attribute
              (A_Setting_Node, "name"));

         Setting_Value :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (DOM.Core.Elements.Get_Attribute
               (A_Setting_Node, "value"));

         -- check if setting already exists and remove it if that
         -- is the case
         if Setting_Hashs.Is_Bound (Target_Map, Setting_Name) then
            Logger.Info ("Config Setting with name: """
              & Ada.Strings.Unbounded.To_String (Setting_Name)
              & """ already read - value replaced by new one: """
              & Ada.Strings.Unbounded.To_String (Setting_Value)
              & """");

            Setting_Hashs.Unbind (Target_Map, Setting_Name);
         end if;

         Setting_Hashs.Bind
            (Target_Map, Setting_Name, Setting_Value);
      end loop;

      -- deallocate used memory for list
      ----------------------------------
      DOM.Core.Free (Setting_Nodes_List);
   end Read_Setting_Entries_Into_Hash_Map;

   ------------------------------------------------------------------------
   procedure Access_Config_File
     (Config_File : in     String;
      Target_Map  : in out Setting_Hashs.Mapping) is

      -- Needed for deallocation
      The_Tree_Reader  : Tree_Readers.Tree_Reader;

      -- top level node (document node)
      The_XML_Document : Dom.Core.Document;
   begin

      -- Load and Parse Config_File
      begin
         XML_File_Access.Load_XML_File_Validated
           (Config_File, The_Tree_Reader, The_XML_Document);
      exception
         when XML_File_Access.XML_File_Access_Error_Exception =>

            Logger.Fatal ("Procedure: Access_Config_File - "
                          & "Cannot access file: " & Config_File);

            raise Config_File_Could_Not_Be_Accessed_Exception;
         when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>

            Logger.Fatal ("Procedure: Access_Config_File - "
                          & "XML Error while parsing file: " & Config_File);

            raise Config_File_Not_Correct_Exception;
      end;

      -- check for correct type
      if (XML_File_Access.Does_XML_Document_Belong_To_Type
          (Config_File_Identifier, The_XML_Document) = False) then

         Logger.Fatal ("Procedure: Access_Config_File - XML File is not"
                        & "of correct type: "
                        & Config_File);
         raise Config_File_Not_Correct_Exception;
      end if;

      begin
         -- read settings into internal data structures
         Read_Setting_Entries_Into_Hash_Map (The_XML_Document, Target_Map);
         -- deallocate memory
         Tree_Readers.Free(The_Tree_Reader);
      exception
         when others =>
            Logger.Fatal ("Procedure: Access_Config_File - XML Error while "
                          & "reading entries of parsed xml file: "
                          & Config_File);

            Tree_Readers.Free(The_Tree_Reader);
            raise Config_File_Not_Correct_Exception;
      end;
   end Access_Config_File;

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Settings
     (GIANT_Config_File : in String;
      User_Config_File  : in String) is

   begin
      Giant_Settings_Map := Setting_Hashs.Create;
      User_Settings_Map  := Setting_Hashs.Create;

      -- Read files
      -------------
      if not (GIANT_Config_File = "") then
         Access_Config_File (GIANT_Config_File, Giant_Settings_Map);
      end if;

      if not (User_Config_File = "") then
         Access_Config_File (User_Config_File, User_Settings_Map);
      end if;

      -- Validate settings
      --------------------
      Validate_Read_Settings (Giant_Settings_Map);
      Validate_Read_Settings (User_Settings_Map);

      -- Add default values for not read settings into Giant_Settings_Map
      -------------------------------------------------------------------
      Add_Default_Values_If_Necessary;

      -- Finshed
      ADO_Initialized := True;
   end Initialize_Config_Settings;

   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is

   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      Setting_Hashs.Destroy (Giant_Settings_Map);
      Setting_Hashs.Destroy (User_Settings_Map);

      ADO_Initialized := False;
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to configuration data.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Setting_Exist (Name : in String)
     return Boolean is
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if Setting_Hashs.Is_Bound
        (Giant_Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name)) or
        Setting_Hashs.Is_Bound
        (User_Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name)) then

         return True;
      end if;

      return False;
   end Does_Setting_Exist;

   ---------------------------------------------------------------------------
   function Get_Setting_As_String (Name : in String)
     return String is
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      -- first search User_Settings_Map
      if Setting_Hashs.Is_Bound
        (User_Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name)) then

        return Ada.Strings.Unbounded.To_String
          (Setting_Hashs.Fetch
            (User_Settings_Map,
             Ada.Strings.Unbounded.To_Unbounded_String (Name)));
      else
        return Ada.Strings.Unbounded.To_String
          (Setting_Hashs.Fetch
            (Giant_Settings_Map,
             Ada.Strings.Unbounded.To_Unbounded_String (Name)));
      end if;
   end Get_Setting_As_String;


   ---------------------------------------------------------------------------
   function Get_Setting_With_Path_Expanded (Name : in String) return String is

   begin
      -- TODO
      return "!";
   end Get_Setting_With_Path_Expanded;


   ---------------------------------------------------------------------------
   function Get_Setting_As_Integer (Name : in String)
     return Integer is

   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;


      begin
         return Integer'Value
           (Get_Setting_As_String (Name));
      exception
         when Constraint_Error =>
            raise Config_Setting_Is_Not_An_Integer_Value;
      end;
   end Get_Setting_As_Integer;

   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in String) is
   begin
      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      -- insert or replace setting in User_Settings_Map
      if Setting_Hashs.Is_Bound
        (User_Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name)) then

         Setting_Hashs.Unbind
           (User_Settings_Map,
            Ada.Strings.Unbounded.To_Unbounded_String (Name));
      end if;

      Setting_Hashs.Bind
         (User_Settings_Map,
          Ada.Strings.Unbounded.To_Unbounded_String (Name),
          Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Set_Setting;

   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in Integer) is
   begin
      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      Set_Setting (Name, Integer'Image (Value));
   end Set_Setting;

   ---------------------------------------------------------------------------
   procedure Store_User_Config_File (File_Name : in String) is

      Config_File : Ada.Text_IO.File_Type;
      User_Settings_Iter : Setting_Hashs.Bindings_Iter;

      Setting_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Setting_Value : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      --  create the file
      -------------------
      Ada.Text_IO.Create
        (Config_File,
         Ada.Text_IO.Out_File,
         File_Name);
      Ada.Text_IO.Set_Output (Config_File);

      --  write xml file header for external dtd
      ------------------------------------------
      Ada.Text_IO.Put_Line
        ("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");
      Ada.Text_IO.Put_Line
        ("<!DOCTYPE giant_config_file");
      Ada.Text_IO.Put_Line
        ("  SYSTEM ""giant_config_file.dtd"">");
      Ada.Text_IO.New_Line;

      --  open top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("<giant_config_file>");

      --  write setting entries
      -------------------------------------------
      User_Settings_Iter :=
        Setting_Hashs.Make_Bindings_Iter (User_Settings_Map);

      while Setting_Hashs.More (User_Settings_Iter) loop
         Setting_Hashs.Next
           (User_Settings_Iter, Setting_Name, Setting_Value);

            Ada.Text_IO.Put_Line
              ("  <setting name  = """
              & Ada.Strings.Unbounded.To_String
                  (Setting_Name)
              & """");
            Ada.Text_IO.Put_Line
              ("           value = """
              & Ada.Strings.Unbounded.To_String
                  (Setting_Value)
              & """ />");
      end loop;

      -- last entry in file - close top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("</giant_config_file>");

      --  close down resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Config_File);
   end Store_User_Config_File;

end Giant.Config_Settings;
