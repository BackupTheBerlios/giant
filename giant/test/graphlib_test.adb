------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und ‹bersetzerbau, University of Stuttgart for
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
--  Based on framework of: Steffen Pingel
--  First Author:          Oliver Kopp
--
--  $RCSfile: graphlib_test.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with Ada.Text_Io;

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Giant.Graph_Lib.Test;
with Giant.Graph_Lib.Node_Attribute_Filters.Test;
with Giant.Graph_Lib.Subgraphs.Test;
with Giant.Graph_Lib.Subgraphs.Atomic_Tests;
with Giant.Default_Logger;

procedure Graphlib_Test is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Giant.Graph_Lib.Test.Test_Case);
      Add_Test (Result,
                new Giant.Graph_Lib.Node_Attribute_Filters.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Atomic_Tests.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Subgraphs.Test.Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Giant.Default_Logger.Init ("debug.log");
   Giant.Default_Logger.Debug ("Starting Test...");

   Run;

   Giant.Default_Logger.Close;
end Graphlib_Test;
