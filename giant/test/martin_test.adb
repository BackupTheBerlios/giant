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
--  $RCSfile: martin_test.adb,v $, $Revision: 1.26 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Giant.Config.Class_Sets.Test;
with Giant.Config.Global_Data.Test;
with Giant.Config.Vis_Styles.Test;
with Giant.File_Management.Test;
with Giant.GSL_Support.Test;
with Giant.Node_Annotations.Test;
with Giant.Projects.Test;
with Giant.Vis_Windows.Test;
with Giant.XML_File_Access.Test;

--  with Hashed_Mappings_Lazy_Init_Test;

with Giant.Default_Logger;

procedure Martin_Test is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      --  Add_Test (Result, new Hashed_Mappings_Lazy_Init_Test.Test_Case);
      Add_Test (Result, new Giant.Config.Class_Sets.Test.Test_Case);
      --  Add_Test (Result, new Giant.Config.Global_Data.Test.Test_Case);
      --  Add_Test (Result, new Giant.Config.Vis_Styles.Test.Test_Case);
      --  Add_Test (Result, new Giant.File_Management.Test.Test_Case);
      --  Add_Test (Result, new Giant.GSL_Support.Test.Test_Case);
      --  Add_Test (Result, new Giant.Node_Annotations.Test.Test_Case);
      --  Add_Test (Result, new Giant.Projects.Test.Test_Case);
      --  Add_Test (Result, new Giant.Vis_Windows.Test.Test_Case);
      --  Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Giant.Default_Logger.Init ("the_martins_test.log");
   Giant.Default_Logger.Debug ("Starting Test...");

   Run;

   Giant.Default_Logger.Close;
end Martin_Test;
