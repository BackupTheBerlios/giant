------------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-xml_file_access-test.adb,v $, $Revision: 1.2 $
--  $Author: schwiemn $
--  $Date: 2003/06/30 20:33:19 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.XML_File_Access;

with Giant.Logger;

package body Giant.XML_File_Access.Test is

   package Logger is new Giant.Logger("Giant.XML_File_Access.Test");

   ---------------------------------------------------------------------------
   procedure Test_Memory_Leacks
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
     
      My_XML_Document : Dom.Core.Document;
      My_Tree_Reader  : Tree_Readers.Tree_Reader;
   begin
      
      for i in 1 .. 50000000 loop
         Giant.XML_File_Access.Load_XML_File_Validated
           ("./resources/node_annotations/node_annotations.xml",
            My_Tree_Reader,
            My_XML_Document);
         
         Assert (Giant.XML_File_Access.Does_XML_Document_Belong_To_Type 
           ("giant_node_annotations_file", My_XML_Document), 
            "Check whether detects correct file");   
         Assert (not Giant.XML_File_Access.Does_XML_Document_Belong_To_Type 
           ("irgendwas", My_XML_Document), 
            "Check whether detects wrong file");   
         
         Tree_Readers.Free (My_Tree_Reader);      
      end loop;
   end Test_Memory_Leacks;
         
   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Giant.XML_File_Access.Test - Basic Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Memory_Leacks'Access, "Memory_Leack_Test");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.XML_File_Access.Test;
