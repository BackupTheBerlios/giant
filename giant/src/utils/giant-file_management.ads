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
-- $RCSfile: giant-file_management.ads,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:14:55 $
--
-- -----------------------------------------------
--
-- This package offers basic functionality used by GIANT to handle files.
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with String_Lists; -- from Bauhaus IML "Reuse.src"

package Giant.File_Management is

   ---------------------------------------------------------------------------
   -- The maximum length of a file name (only the name - no path)
   Max_File_Name_Length : constant integer := 256;

   ---------------------------------------------------------------------------
   -- Raised if an not existing of incorrect directory is passed
   -- as parameter
   Invalid_Directory_Exception : exception;

   ---------------------------------------------------------------------------
   -- Returns file names from a Directory.
   -- Each returned unbounded String in the list corresponds
   -- to the string returned by the function
   -- "function Name(File : in File_Type) return String;"
   -- from the package "Ada.Sequential_IO";
   -- It is not absolute certain but string is expected to be
   -- the absolute path of the file.
   -- This string is specified in the Ada 95 Reference Manual
   -- (according to the International Standard ISO/IEC 8652:1995(E)).
   --
   -- See ARM "A.8.2 File Management":
   -- 22 "Returns a string which uniquely identifies the external file
   --     currently associated with the given file (and may thus be used in
   --     an Open operation). If an external environment allows alternative
   --     specifications of the name (for example, abbreviations), the
   --     string returned by the function should correspond
   --     to a full specification of the name."
   --
   -- If Filter = True, then
   -- only Files which name ends with the Filter String are returned.
   -- E.g. Filter_String = ".xml" returns all Files "*.xml"
   -- in the given directory.
   -- If Filter = FALSE then all Files in the directory are
   -- returned regardeless of the ending.
   --
   -- Parameter:
   --   Path_To_Dir - A Path to a directory
   --     where the files should be searched.
   --   Filter - Determines whether all files found in the directory
   --     should be returned (False) or whether only the files
   --     with the ending passed in "Filter_String" should be
   --     returned (True).
   --   Filter_String - If "Filter" is "True" then only files
   --   which end with the string "Filter_String" are returned.
   -- Return:
   --   A List of all file names (incl. absolute path)
   --   that comply to the filter criterion.
   --   The returned list may be empty if no appropriate files
   --   were found.
   -- Raises:
   --   Invalid_Directory_Exception - Raised if the passed Path "Path_To_Dir"
   --   is no correct or existing path.
   function Get_Filtered_Files_From_Directory
     (Path_To_Dir    : in String;
      Filter         : in Boolean;
      Filter_String  : in String)
     return String_Lists.List;
	 
   ---------------------------------------------------------------------------
   -- Raised if a file does not exist
   File_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if an existing directory path could not be calculated out
   -- of a path to a file name.
   Directory_Could_Not_Be_Calculated_Exception : exception;

   ---------------------------------------------------------------------------
   -- Returns the "path" out of a string holding a file name including
   -- a path.
   --
   -- Only the path must exist. It is not checked whether the file exists
   -- too.
   -- If the passed path in "File_Path" is relative then a relative path
   -- to the directory will be returned if it is absolute then a absolute
   -- path will be returned.
   --
   -- Parameters:
   --   File_Path - A File Name String that holds a file name and a path.
   -- Returns:
   --   The Path to the directory there the file is loacted.
   --   If only a file name is passed (without a path) then the
   --   working directory for the current execution environment
   --   will be returned.
   -- Raises:
   --   Directory_Could_Not_Be_Calculated_Exception - Raised if the
   --   directory that is part of the file name "File_Path" does
   --   not exist.
   function Return_Dir_Path_For_File_Path (File_Path : in String) 
     return String;

   ---------------------------------------------------------------------------
   -- For a given realtive Path to a file and a path to a directory there the
   -- relative path may begin, this subprogram calculates the aboslute path
   -- to that file as returned by:
   -- ---
   -- "function Name(File : in File_Type) return String;"
   -- from the package "Ada.Sequential_IO";
   -- ---
   -- For further Details see internal docu
   -- "function Get_Filtered_Files_From_Directory".
   --
   -- Parameters:
   --   Start_Dir - The directory there the relative path
   --     "Relative_Path_To_File" begins. "Start_Dir" may also
   --     be a relative path - then the absolute path will
   --     be calculated based on the "current working directory
   --     of the execution environment".
   --   Relative_Path_To_File - A relative path to a file
   --     having "Start_Dir" as root.
   --     If an absolute path is passed, this will cause no problems
   --     (then an equivalent absolute path will be returned).
   --
   -- Returns:
   --    A absolute Path to the File that is calculated based on
   --    "Start_Dir" and "Relative_Path_To_File".
   -- Raises:
   --   File_Does_Not_Exist_Exception - Raised if Start_Dir and
   --     Relative_Path_To_File do together not describe a path
   --     to an existing file.
   function Get_Absolute_Path_To_File_From_Relative
     (Start_Dir             : in String;
      Relative_Path_To_File : in String)
     return String;

   ---------------------------------------------------------------------------
   -- This procedure changes the current working directory for the execution
   -- environment so that it matches the directory there the Executable
   -- file of the program is located.
   procedure Set_Currunt_Working_Dir_To_Exec_Dir;

end Giant.File_Management;
