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
-- $RCSfile: giant-valid_names.ads,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:14:55 $
--
-- -----------------------
-- This package provides the functionality needed
-- to "enforce" the string "standard name" as specified in
-- GIANT Specification "3.3.1.1. Der String "STANDARD NAME"
--
with Ada.Strings.Unbounded;

package Giant.Valid_Names is

   ---------------------------------------------------------------------------
   -- The minimum length (characters) of a "Standard Name"
   Min_Name_Length : constant Integer := 1;

   ---------------------------------------------------------------------------
   -- The maximum length (characters) of a "Standard Name"
   Max_Name_Length : constant Integer := 256;

   ---------------------------------------------------------------------------
   -- Describes the standard name
   type Standard_Name is private;

   ---------------------------------------------------------------------------
   -- Raised if string is passed that does not correspond to the requirements
   -- of "Standard Name" (see GIANT Specification
   -- "3.3.1.1. Der String "STANDARD NAME").
   No_Correct_Standard_Name_Exception : exception;

   ---------------------------------------------------------------------------
   -- Determines whether a string corresponds to the requirements of
   -- "Standard Name".
   --
   -- Parameters:
   --   String_Value - The string that should be checked.
   -- Returns:
   --   True, if "String_Value" may be converted to standard name; False,
   --   otherwise.
   function Is_String_A_Correct_Standard_Name
     (String_Value : in String) return Boolean;

   ---------------------------------------------------------------------------
   -- Converts a String into "Standard Name".
   --
   -- Parameters:
   --   String_Value - The string that should be converted.
   -- Returns:
   --   A "Standard Name" String with the same content as passed by
   --   "String_Value".
   -- Raises
   --   No_Correct_Standard_Name_Exception - Raised if the passed String
   --   "String_Value" may not be converted to "standard name" as it does
   --   not correspond to the requirements for "standard name"
   function To_Standard_Name
     (String_Value : in String) return Standard_Name;

   ---------------------------------------------------------------------------
   -- Coverts "Standard Name" to String.
   --
   -- Parameters:
   --   Name - The Standard Name that should be converted to a String.
   -- Returns:
   --   A String with the same content as "Name".
   function To_String
     (Name : in Standard_Name) return String;

   ---------------------------------------------------------------------------
   -- Raised if the name calculated out of a file name is no correct
   -- "Standard name" (see GIANT Specification "3.3.1.1.
   -- Der String "STANDARD NAME")
   No_Correct_Standard_Name_Calculated_Exception : exception;

   ---------------------------------------------------------------------------
   --
   -- Calculates a name out of a file name by neglecting the path and the
   -- ending that may be part of a file name,
   -- i.e. the ending (all characters after the last dot "." incl. the dot
   -- itself and the path is removed from "File_Name".
   --
   -- The calculated name must be valid according to the affordences of
   -- "Standard_Name".
   --
   -- Paramters:
   --   File_Name - The full name (optional incl. path) of a file.
   -- Returns:
   --   The Name calculated for that file.
   -- Raises:
   --   No_Correct_Standard_Name_Calculated_Exception - Raised if the
   --     calculated name is no standard name.
   -- Examples:
   --   - "./test/my_file.xml" --> "my_file"
   --   - "a.data"             --> "a"
   --   - "./../../data"       --> "data"
   function Calculate_Name_For_File (File_Name : in String)
     return Standard_Name;



------------------------------------------------------------------------------
private

   type Standard_Name is new Ada.Strings.Unbounded.Unbounded_String;

end Giant.Valid_Names;
