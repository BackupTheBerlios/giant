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
-- $RCSfile: giant-xpm_file_access.adb,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:02:19 $
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Giant.XPM_File_Access is
   
   ---------------------------------------------------------------------------
   -- Read a file into Gtkada.Types.Chars_Ptr_Array inside of GIANT
   -- you should only use absolute paths for "File_Name"
   -- because the working directory for the current execution environment 
   -- may differ from the directory of the config file where a relative
   -- path is read.
   function Read_Pixmap_File (File_Name : String)
     return Gtkada.Types.Chars_Ptr_Array is
	 
      File_Content_Unb : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
		
	  A_Char : Character;

	  File : Ada.Text_IO.File_Type;  
	  
	  -- This datastrcture is used to store the file in the main
	  -- memory
	  The_Char_Ptr_Array : Gtkada.Types.Chars_Ptr_Array :=
        Gtkada.Types.Null_Array;
	     
   begin
	 
	  begin
         Ada.Text_IO.Open
	       (File, 
		    ADA.Text_IO.In_File, 
		    File_Name);			
      exception			
	     when others =>
		 raise XPM_File_Access_Error_Exception;
	  end;
		  
      while not Ada.Text_IO.End_Of_File (File) loop
		  
         Ada.Text_IO.Get (File, A_Char);		  
		 Ada.Strings.Unbounded.Append (File_Content_Unb, A_Char);   		     
	  end loop;
		  
      The_Char_Ptr_Array := 
        Gtkada.Types."+" 
		  (The_Char_Ptr_Array,
	        Ada.Strings.Unbounded.To_String (File_Content_Unb));
				 
   	  Ada.Text_IO.Close (File); 
	  
	  return The_Char_Ptr_Array;  
   end Read_Pixmap_File;
	 
end Giant.XPM_File_Access;
