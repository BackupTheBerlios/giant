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
-- $RCSfile: giant-config.ads,v $, $Revision: 1.5 $
-- $Author: squig $
-- $Date: 2003/06/19 16:38:06 $
--
-- -----
-- This package holds the functionality needed to access the
-- data stored in the global configuration file
-- (see GIANT Specification "13.2 Die globale Konfigurationsdatei").
-- Therefore the global config file is represented as an abstract data
-- object (ADO).
--
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Gtkada.Types; -- from GTK

package Giant.Config is


   ---------------------------------------------------------------------------
   -- Required settings.
   -- A List of setting identifiers. These settings must be known to
   -- the config ADO after parsing both config files
   -- (GIANT_Config_File, User_Config_File);
   type Required_Settings_Type is array (integer range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   Required_Settings : constant Required_Settings_Type :=
     (Ada.Strings.Unbounded.To_Unbounded_String
        ("Icon_For_Node_Annotations"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("Actual_Selection_Highlight_Color"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("Selection_Highlight_Color_1"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("Selection_Highlight_Color_2"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("Selection_Highlight_Color_3"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("IML_Subgraph_Highlight_Color_1"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("IML_Subgraph_Highlight_Color_2"),
      Ada.Strings.Unbounded.To_Unbounded_String
        ("IML_Subgraph_Highlight_Color_3"));

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
   -- Raised if a passed config file could not be parsed as a
   -- valid xml file.
   Config_File_Not_Correct_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised when a required config setting is not found after reading
   -- both config files.
   Required_Config_Setting_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a passed config file could not be opened for any reasons
   -- (e.g. it is not being found).
   Config_File_Could_Not_Be_Accessed_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a config setting identified by its unique name does not
   -- exist.
   Config_Setting_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised when a config setting recalled as a integer value can not
   -- be converted to an integer value.
   Config_Setting_Is_No_Integer_Value_Exception : exception;

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
   -- Initializes the ADO and reads the configuration data from the given
   -- config files.
   --
   -- First, the settings from "GIANT_Config_File" are loaded
   -- Second, the settings from  "User_Config_File" are loaded
   --
   -- Each setting has a unique identifier - a name string.
   --
   -- Relative Paths inside a config file will be regarded as relative
   -- towards the directory of the config file.
   --
   -- The settings are read in sequential order following the
   -- xml node hierarchy. First the file "GIANT_Config_File" is
   -- read, second the file "User_Config_File".
   -- If a setting with the same indentifier is found
   -- twice (in GIANT_Config_File and in User_Config_File)
   -- the setting read from GIANT_Config_File is replaced
   -- by the setting read from User_Config_File;
   --
   -- Parameters:
   --   GIANT_Config_File - The file (filename and path) where
   --     the GIANT config file is stored. Such a config file must
   --     exist.
   --     ONLY ABSOLUTE PATHS WILL WORK.
   --
   --   User_Config_File - A userdefined config file.
   --     "User_Config_File may be an empty string (""), then this parameter
   --      will be ignored and no user defined config file will be loaded.
   --      ONLY ABSOLUTE PATHS WILL WORK.
   --
   -- Raises:
   --   Required_Config_Setting_Not_Found_Exception - Raise when none
   --     of the two passed coonfig files holds an required config setting
   --     (as defined in the array "Required_Settings").
   --   Config_File_Not_Correct_Exception - Raised when the file passed
   --     (parameter "GIANT_Config_File" and "User_Config_File") is
   --     not a correct (valid) config file.
   --   Config_File_Could_Not_Be_Accessed_Exception - Raised if
   --     if the passed config file could not be accessed by a file
   --     reader.
   procedure Initialize_Config_Data
     (GIANT_Config_File : in String;
      User_Config_File  : in String);

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
   -- General Access to configuration data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This function is used to determine whether a config setting
   -- exists or not.
   --
   -- Parameters:
   --   Name_Of_Setting - The name (each setting has an unique name) of
   --     the setting.
   -- Returns:
   --   True, if the config setting exists; False, otherwise.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   function Does_Setting_Exist (Name_Of_Setting : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   -- This method returns a config setting as a string.
   -- As described above each config setting is identified by a unique
   -- name (a sting).
   --
   -- Parameters:
   --   Name_Of_Setting - The unique identifier of a config setting.
   -- Returns:
   --   The the config setting corresponding to "Name_of_Setting"
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name_of_Setting";
   function Return_Setting_As_String (Name_Of_Setting : in String)
     return Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------
   -- This method returns a config setting as a integer value.
   -- As described above, each config setting is identified by a unique
   -- name (a sting).
   --
   -- Parameters:
   --   Name_Of_Setting - The unique identifier of a config setting.
   -- Returns:
   --   The config setting corresponding to "Name_of_Setting"
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   --   Config_Setting_Is_No_Integer_Value_Exception - raised if the
   --     config setting could not be converted to Integer;
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name_of_Setting";
   function Return_Setting_As_Integer (Name_Of_Setting : in String)
     return Integer;


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
   -- Return:
   --   A pointer to an array of character pointers.
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
