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
-- $RCSfile: giant-config.ads,v $, $Revision: 1.7 $
-- $Author: schwiemn $
-- $Date: 2003/06/20 20:33:57 $
--
-- -----
-- This package holds the functionality needed to access the
-- data stored in the global configuration file
-- (see GIANT Specification "13.2 Die globale Konfigurationsdatei").
--
-- This package simply uses the information from the package
-- Giant.Config_Settings and does some further processing in order
-- to simplify the access on the information.
--
-- The purpose of this packe is to other necessary config settings in
-- a performant and simple way.
--
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Gtkada.Types; -- from GTK

package Giant.Config is


   ---------------------------------------------------------------------------
   -- A String Pointer used to describe a Color.
   -- This pointer structure is used due to
   -- performance requirements, as the
   -- functions returning this type are called
   -- for each window node that has to be visualized.
   --
   -- The string describing the color (the result of
   -- function Get_Color_Value) is not specified.
   -- The reason for that ist that the config files should
   -- be independent of a special color model.
   -- The packages using this type will have to check
   -- whether it describes a valid color according to their
   -- color model.
   type Color_Access is private;

   ---------------------------------------------------------------------------
   -- According to the specification, there are three different
   -- userdefined colors used to higlight Selections
   -- (see GIANT Specification "13.2.2 Farbe von Hervorhebungen").
   type Selection_High_Light_ID is (Color_1, Color_2, Color_3);

   ---------------------------------------------------------------------------
   -- There are also three different colors used to highlight
   -- IML_Subgrapghs.
   type IML_Subgraph_High_Light_ID is (Color_1, Color_2, Color_3);

   ---------------------------------------------------------------------------
   -- Icons are asked for very frequently and the character array
   -- that holds the data describing an icon is quite big; therefore,
   -- for the sake of performance, it can not be passed as value
   -- parameter. Instead, a pointer to the internal datastructure
   -- is passed.
   --
   -- By no means you are allowed to use this pointer for any kind
   -- of deallocation.
   type Chars_Ptr_Array_Access is
     access Gtkada.Types.Chars_Ptr_Array;

   ---------------------------------------------------------------------------
   -- Raised if an uninitialized instance of the ADT Color_Access
   -- is passed.
   Color_Access_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if subprograms are called before the ADO that holds the config
   -- was initialized.
   Config_ADO_Not_Initialized_Exception : exception;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Initializes the ADO and processes the information hold in
   -- the ADO Giant.Config_Settings
   --
   -- ADO Giant.Config_Settings must have been initialized before this
   -- subprogramm is called.
   procedure Initialize_Config_Data;

   ---------------------------------------------------------------------------
   -- Finalizes the ADO;
   -- Deallocates all memory used by the ADO.
   -- After the call of this procedure "Initialize_Config_Data" may be
   -- called without causing memory leaks.
   -- After the call the ADO is regarded as not initialized.
   --
   -- Note
   --  The types "Chars_Ptr_Array_Access" and "Color_Access", that
   --  are returned by some functions of this package, are pointers
   --  that point directly into the internal data structure of
   --  the ADO. Because of that you should not use these pointers
   --  after the finalisation of the ADO, otherwise
   --  you will definitely have dangling pointers
   --
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   procedure Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to processed config data.
   ---------------------------------------------------------------------------
   
   ---------------------------------------------------------------------------
   -- Raised if the Resource Directory Could not be calculated.
   Invalid_Resource_Directory_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Returns an absolute path for the resources directory. If the
   --   corresponding setting in  Giant.Config_Settings holds an
   --   realtive path (that is NOT RECOMMENDED) this path will
   --   be expanded using "Root_Path".
   --
   -- Returns:
   --   An absolute path that ends with a directory separator
   --   (e.g. "/home/donald/resources/").

   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   --   Invalid_Resource_Directory_Exception - raised if
   --     if (e.g. due to an not correct path entry in the config file) 
   --     such a path could not be calculated.
   function Get_Resources_Directory (Root_Path : in String) return String;

   ---------------------------------------------------------------------------
   -- C
   -- Access to the configuration data about colors.
   --
   -- The following functions return data needed to visualize the iml graph
   -- in visualisation windows. The implementation is not performance
   -- optimized as this settings only needs to be read once.
   --
   -- Note: Color_Access
   -- The functions returning "Color_Access" do not guarentee that
   -- for the same color the pointer "Color_Access" points to the
   -- same object on the heap.
   -- So for two equal colors, C1 = C2 is not guaranted
   -- (C1: Color_Access; C2: Color_Access).
   -- This does not apply to the functions of the subpackage
   -- Config.Vis_Styles (see internal documentation of this package).
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Used to initialize the ADT "Color_Access".
   --
   -- Returns the String describing the highlight color for the
   -- actual selection.
   -- As this function is not supposed to be used frequently, the
   -- implementation is not very performant.
   --
   -- Note
   --   The returned value is a pointer. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   -- Returns:
   --   A pointer to a String which describes a color.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --   is called before "Initialize_Config_Data".
   function Return_Highlight_Color_For_Actual_Selection
     return Color_Access;

   ---------------------------------------------------------------------------
   -- This function returns the color used to highlight selections.
   -- As this function is not supposed to be used frequently, the
   -- implementation is not very performant.
   --
   -- Note
   --   The returned value is a pointer. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   -- Parameters:
   --   Highlight_ID - As there are several different colors to highlight
   --     more than one selection at once, this parameter is used to
   --     determine which highlight color should be returned.
   -- Returns:
   --   A pointer to a String which describes a color.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   function Return_Highlight_Color_For_Selection
     (Highlight_ID : in Selection_High_Light_ID)
     return Color_Access;

   ---------------------------------------------------------------------------
   -- This function returns the color used to highlight selections.
   -- As this function is not supposed to be used frequently, the
   -- implementation is not very performant.
   --
   -- Note
   --   The returned value is a pointer. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   -- Parameters:
   --   Highlight_ID - As there are several different colors to highlight
   --     more than one selection at once, this parameter is used to
   --     determine which highlight color should be returned.
   -- Returns:
   --   A pointer to a String which describes a color.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   function Return_Highlight_Color_For_IML_Subgraph
     (Highlight_ID : in IML_Subgraph_High_Light_ID)
     return Color_Access;

   ---------------------------------------------------------------------------
   -- Returns the Icon used to show that a window node has an annotation.
   --
   -- Note
   --   The returned value is a pointer into the internal
   --   datastructure of this ADO. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   --   You MAY NOT use the returned pointer for DEALLOCATION!
   --
   --   A relative path describing the position of the file (read from
   --   the config files) will be expanded using the 
   --   RESOURCES_DIRECTORY.
   --
   -- Returns:
   --   A pointer to an array of character pointers.
   --   Will return a NULL POINTER,
   --   if now Icon was found (e.g. as the file read from the config settings
   --   could not be accessed).
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   function Return_Icon_For_Node_Annotations
     return Chars_Ptr_Array_Access;


   ---------------------------------------------------------------------------
   -- D
   -- Color_Access
   -- The following functions provide read access to the abstract data type
   -- "Color_Access".
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This function returns a string value for the data object describing
   -- a color.
   -- The content of the returned String is not specified, as
   -- this package is independent from color models, the
   -- package using this package has to check whether the String
   -- is correct according to its color model.
   --
   -- Parameter:
   --   Color_Ptr - The Pointer describing a color value.
   -- Return:
   --   The corresponding color value.
   -- Raises:
   --   Color_Access_Not_Initialized_Exception - raised if an instance
   --     of Color_Access that is not initialized is passed as paraneter.
   function Get_Color_Value (Color_Ptr : in Color_Access) return String;

   ---------------------------------------------------------------------------
   -- This function calculates a hash value for Color_Access.
   --
   -- Note:
   --   The Hash Value is calculated based on the pointer Color_Access.
   --   That means that the Hash Value could be different for two
   --   instances of Color_Access even if the result of Get_Color_Value
   --   is equal.
   -- Parameter:
   --   The Color_Access Instance for that the Hash-Value should be
   --   calculated.
   -- Return:
   --   A Integer hash value.
   -- Raises:
   --   Color_Access_Not_Initialized_Exception - raised if an instance
   --     of Color_Access that is not initialized is passed as paraneter.
   function Hash_Color_Access (Color_Ptr : in Color_Access) return Integer;

------------------------------------------------------------------------------
private

   -- A String representing a Color Value.
   -- The Content of this string is not checked by
   -- this package.
   --
   -- Needs to be a pointer to "Unbounded_String" as a pointer to
   -- "String" has 64 Bit size what makes hashing this pointer not quite
   -- easy.
   type Color_Access is access all Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------
   -- Deallocates Color_Access
   -- - needed by this package and subpackages for deallocation.
   procedure Free_Color_Access is new Ada.Unchecked_Deallocation
     (Ada.Strings.Unbounded.Unbounded_String, Color_Access);

   ---------------------------------------------------------------------------
   -- Deallocate the pointer "Chars_Ptr_Array_Access" to a "Chars_Ptr_Array"
   --   - needed by this package and subpackages for deallocation.
   -- Note
   --   This does not deallocate the momory allocated by the
   --   Chars_Ptr_Array itself.
   --   Therefore you should use Gtkada.Types ->
   --   "procedure Free (A : in out Chars_Ptr_Array)"
   procedure Free_Chars_Ptr_Array_Access is new Ada.Unchecked_Deallocation
     (Gtkada.Types.Chars_Ptr_Array, Chars_Ptr_Array_Access);

end Giant.Config;
