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
-- $RCSfile: giant-config.adb,v $, $Revision: 1.5 $
-- $Author: schwiemn $
-- $Date: 2003/06/20 20:33:57 $
--
with Ada.Unchecked_Deallocation;

with Ordered_Sets; -- from Bauhaus IML "Reuse.src"

with Giant.Ptr_Normal_Hashs; -- from GIANT
pragma Elaborate_All (Giant.Ptr_Normal_Hashs);
with Giant.XML_File_Access;  -- from GIANT
with Giant.XPM_File_Access;  -- from GIANT
with Giant.File_Management;  -- from GIANT

with Tree_Readers;       -- from xmlada
with DOM.Core;           -- from xmlada
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
   -- 0.2
   -- Internal subprograms
   ---------------------------------------------------------------------------

   function Is_Config_ADO_Initialized return Boolean is   
   begin
      return ADO_Initialized;
   end Is_Config_ADO_Initialized;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Data is
   begin
   
   
     -- TODO
     null;


   end Initialize_Config_Data;

   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is

   begin
   
   -- TODO
   null;

  --    if (Is_Config_ADO_Initialized = False) then

   --      raise Config_ADO_Not_Initialized_Exception;
   --   end if;

      -- Free set holding config settings
   --   Setting_Data_Sets.Destroy(ADO_Free_Defined_Settings_Set);

      -- Free memory used to store the node annotation icon
   --   Gtkada.Types.Free (Node_Annotation_Icon.all);

      -- Free pointer
  ---    Free_Chars_Ptr_Array_Access (Node_Annotation_Icon);

      -- Free momory used to store color strings
  --    Free_Color_Access (Actual_Selection_Highlight_Color);

  --    For I in Selection_Highlight_Colors'Range loop

  --       Free_Color_Access (Selection_Highlight_Colors (I));
  --    end loop;

   --   For I in IML_Subgraph_High_Light_Colors'Range loop

   --      Free_Color_Access (IML_Subgraph_High_Light_Colors (I));
   --   end loop;

      -- Mark ADO as not initialized
   --   ADO_Initialized := False;
   
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to processed config data.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Resources_Directory (Root_Path : in String) return String is
   
   begin 
   
   raise Invalid_Resource_Directory_Exception;
   return "!";
   
   end Get_Resources_Directory;

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

-- TODO
      return null;
   end Return_Highlight_Color_For_Actual_Selection;

   ---------------------------------------------------------------------------
   function Return_Highlight_Color_For_Selection
     (Highlight_ID : in Selection_High_Light_ID)
     return Color_Access is

   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

-- TODO
   return null;

--      return Selection_Highlight_Colors (Highlight_ID);
   end Return_Highlight_Color_For_Selection;

   ---------------------------------------------------------------------------
   function Return_Highlight_Color_For_IML_Subgraph
     (Highlight_ID : in IML_Subgraph_High_Light_ID)
     return Color_Access is
   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

-- TODO
return null;
--      return IML_Subgraph_High_Light_Colors (Highlight_ID);
   end Return_Highlight_Color_For_IML_Subgraph;

   ---------------------------------------------------------------------------
   function Return_Icon_For_Node_Annotations
     return Chars_Ptr_Array_Access is

   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

-- TODO
      return null;
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
