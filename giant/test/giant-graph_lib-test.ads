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
--  $RCSfile: giant-graph_lib-test.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--
------------------------------------------------------------------------------
--
--  Provides an aunit test.
--

with Ada.Strings.Unbounded;

with AUnit.Test_Cases;

package Giant.Graph_Lib.Test is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   
   --  Registers routines to be run.
   procedure Register_Tests (T : in out Test_Case);
   
   --  Provides name identifying the test case.
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;
   
   --  Preparation performed before each routine.
   procedure Set_Up (T : in out Test_Case);
   
   --  Cleanup performed after each routine.
   procedure Tear_Down (T :  in out Test_Case);
   
end Giant.Graph_Lib.Test;
