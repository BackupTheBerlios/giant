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
--  First Author: Steffen Pingel
--
--  $RCSfile: framework_test.adb,v $, $Revision: 1.17 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:57 $
--
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Hashed_Mappings_Test;

with Giant.Config.Class_Sets.Test;
with Giant.Config.Test;
with Giant.Config.Vis_Styles.Test;
with Giant.Default_Logger;
with Giant.Default_Logger.Test;
with Giant.File_Management.Test;
with Giant.Graph_Lib.Test;
with Giant.Graph_Lib.Node_Attribute_Filters.Test;
with Giant.Graph_Lib.Subgraphs.Atomic_Tests;
with Giant.Graph_Lib.Subgraphs.Test;
with Giant.Gsl.Test;
with Giant.Gsl_Support.Test;
with Giant.Layout_Factory.Test;
with Giant.Matrix_Layouts.Test;
with Giant.Node_Annotations.Test;
with Giant.Projects.Test;
with Giant.Tree_Layouts.Test;
with Giant.Valid_Names.Test;
with Giant.Vis_Windows.Test;
with Giant.XML_File_Access.Test;

procedure Framework_Test is

   function Default_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      --Add_Test (Result, new );
      Add_Test (Result, new Giant.Config.Class_Sets.Test.Test_Case);
      Add_Test (Result, new Giant.Config.Test.Test_Case);
      Add_Test (Result, new Giant.Config.Vis_Styles.Test.Test_Case);
      Add_Test (Result, new Giant.Default_Logger.Test.Test_Case);
      Add_Test (Result, new Giant.File_Management.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Test.Test_Case);
      Add_Test
        (Result, new Giant.Graph_Lib.Node_Attribute_Filters.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Atomic_Tests.Test_Case);
      Add_Test (Result, new Giant.Gsl.Test.Test_Case);
      Add_Test (Result, new Giant.Gsl_Support.Test.Test_Case);
      Add_Test (Result, new Giant.Layout_Factory.Test.Test_Case);
      Add_Test (Result, new Giant.Matrix_Layouts.Test.Test_Case);
      Add_Test (Result, new Giant.Node_Annotations.Test.Test_Case);
      Add_Test (Result, new Giant.Projects.Test.Test_Case);
      Add_Test (Result, new Giant.Tree_Layouts.Test.Test_Case);
      Add_Test (Result, new Giant.Valid_Names.Test.Test_Case);
      Add_Test (Result, new Giant.Vis_Windows.Test.Test_Case);
      Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
      return Result;
   end Default_Suite;

   function Reuse_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Hashed_Mappings_Test.Test_Case);
      return Result;
   end Reuse_Suite;

   function Memory_Leak_Suite
     return Access_Test_Suite
   is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
      return Result;
   end Memory_Leak_Suite;

   procedure Run is new AUnit.Test_Runner (Default_Suite);

begin
   Giant.Default_Logger.Init ("debug.log");
   Giant.Default_Logger.Info ("Running Tests...");

   Run;

   Giant.Default_Logger.Close;
end Framework_Test;
