------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-xml_file_access-test.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.XML_File_Access;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

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
