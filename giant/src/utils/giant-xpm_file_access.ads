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
-- $RCSfile: giant-xpm_file_access.ads,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:02:19 $
-- --------
-- 
-- Offers functionality to hande xpm files.
--
with Gtkada.Types; -- from GTK

package Giant.XPM_File_Access is
   
   ---------------------------------------------------------------------------
   -- Raised if something goes wrong while trying to open a passed file.
   XPM_File_Access_Error_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Reads a given file into the main memory. This function is used 
   -- by GIANT to load xpm files describing node icons etc.
   --
   -- Note
   --   It is not checked whether the passed file is a correct xpm file.
   --   You are responsible for the deallocation of the result.
   --
   -- Parameters:
   --   File_Name - A Path to a xpm file that should be read into the
   --     main memory. An absolute path is required.
   -- Returns:
   --   A Chars_Ptr_Array that represents the content of the
   --   passed file in the main memory.
   -- Raises:
   --   XPM_File_Access_Error_Exception - Raised if something goes
   --     wrong while accessing the file "File_Name".
   function Read_Pixmap_File (File_Name : String)
     return Gtkada.Types.Chars_Ptr_Array;
	 
end Giant.XPM_File_Access;
