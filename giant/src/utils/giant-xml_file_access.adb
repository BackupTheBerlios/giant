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
-- $RCSfile: giant-xml_file_access.adb,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:02:19 $
--
package body Giant.XML_File_Access is

   ---------------------------------------------------------------------------
   procedure Load_XML_File_Validated
     (File         : in String;
      Tree_Reader  : in out Tree_Readers.Tree_Reader;
      XML_Document : out Dom.Core.Document) is

      -- The input file
      Input_File   : Input_Sources.File.File_Input;

   begin

      begin 
        -- open input file
        Input_Sources.File.Open (File, Input_File);
		
      exception
	     when others =>
		    raise  XML_File_Access_Error_Exception;
	  end;

      -- activate "validation feature"
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Validation_Feature, True);
		
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Namespace_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.External_General_Entities_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.External_Parameter_Entities_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Parameter_Entities_Feature, True);

      -- read tree from file completely into main memory
	  -- this may cause exceptions
      Tree_Readers.Parse (Tree_Reader, Input_File);

      -- close input file
      Input_Sources.File.Close (Input_File);

      -- The first element of the document the "Document-Element" or
	  -- "top level node" as it is also called.
      XML_Document := Tree_Readers.Get_Tree (Tree_Reader);
	  
   exception 
   	
	 -- deallocate storrage and close file
     when E : Sax.Readers.XML_Fatal_Error =>
          Input_Sources.File.Close (Input_File);
          Tree_Readers.Free (Tree_Reader);
		  
		  raise XML_File_Parse_Fatal_Error_Exception;
   end Load_XML_File_Validated;

   ---------------------------------------------------------------------------
   function Does_XML_Document_Belong_To_Type
     (Type_Name           : in String;
      Parsed_XML_Document : in Dom.Core.Document) return Boolean is

      Top_Level_Element : DOM.Core.Element;

   begin

      Top_Level_Element :=
        DOM.Core.Documents.Get_Element(Parsed_XML_Document);

      if (Type_Name = DOM.Core.Nodes.Node_Name(Top_Level_Element)) then
         return True;
      end if;

      return False;
   end Does_XML_Document_Belong_To_Type;

end Giant.XML_File_Access;
