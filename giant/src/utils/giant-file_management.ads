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
-- $RCSfile: giant-file_management.ads,v $, $Revision: 1.6 $
-- $Author: schwiemn $
-- $Date: 2003/06/20 13:45:48 $
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
   -- Raised if a file could not be deleted (not existing, not sufficient 
   -- rights, no regular file ...)
   File_Cannot_Be_Deleted_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised if a directory does not exist
   Directory_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if an existing directory path could not be calculated out
   -- of a path to a file name.
   Directory_Could_Not_Be_Calculated_Exception : exception;
      
   ---------------------------------------------------------------------------
   -- Deletes a File.
   --
   -- Parameters:
   --   File_Name - The file (incl. Path) that should be deleted.
   -- Raises 
   --   File_Cannot_Be_Deleted_Exception - Raised if the file could not be
   --   found, accessed or deleted.
   procedure Delete_File (File_Name : in String);

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
   --  Calculates an absolute path from an relative one for a directory.
   --
   --  Parameters:
   --    Start_Dir - The directory there the relative path
   --      "Rel_Dir_Path" begins. "Start_Dir" may also
   --      be a relative path - then the absolute path will
   --      be calculated based on the "current working directory
   --      of the execution environment".
   --    Rel_Dir_Path - A relative path to a directory. If an absolute path is
   --      passed the path will not be changed.
   --  Returns:
   --    An absolute Path for the passed relative path.
   --  Raises:
   --    Directory_Does_Not_Exist_Exception - Raised if Start_Dir and
   --      Rel_Dir_Path do not describe an existing directory. 
   function Get_Absolute_Path_To_Directory_From_Relative
     (Start_Dir    : in String;
      Rel_Dir_Path : in String)
     return String;

   ---------------------------------------------------------------------------
   -- This procedure changes the current working directory for the execution
   -- environment so that it matches the directory there the Executable
   -- file of the program is located.
   procedure Set_Currunt_Working_Dir_To_Exec_Dir;
   
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
   -- Calculates a name out of a file name by neglecting the path and the
   -- ending that may be part of a file name,
   -- i.e. the ending (all characters after the last dot "." incl. the dot
   -- itself and the path is removed from "File_Name".
   --
   -- Does not check whether the file realy exists.
   --
   -- Paramters:
   --   File_Name - The full name (optional incl. path) of a file.
   -- Returns:
   --   The Name calculated for that file.
   -- Examples:
   --   - "./test/my_file.xml" --> "my_file"
   --   - "a.data"             --> "a"
   --   - "./../../data"       --> "data"
   function Calculate_Name_For_File (File_Name : in String)
     return String;

   ---------------------------------------------------------------------------
   -- Appends a directory separator if 
   -- necessary (the passed String should be a path).
   --
   -- Example:
   -- "c:\my_dir" --> "c:\my_dir\" (for Windows Users)
   -- "./dir"     --> "./dir/"
   -- "/dir/"     --> "/dir/"
   --
   -- Parameters:
   --   Directory - A String describing a directory.
   -- Returns:
   --   A path ending with a directory separator. 
   function Append_Dir_Separator_If_Necessary
     (Directory : in String)
     return String;

end Giant.File_Management;
