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
-- First Author: Gerrit Schulz
--
-- $RCSfile: giant-gsl-interpreters.adb,v $
-- $Author: schulzgt $
-- $Date: 2003/06/06 20:02:22 $
--
-- This package implements the datatypes used in GSL.
--

package body Giant.Gsl.Interpreters is

   ---------------------------------------------------------------------------
   --
   function Create_Interpreter return Interpreter is

      Individual : Interpreter;
   begin
      Individual := new Interpreter_Record;
      Individual.Gsl_Compiler := Giant.Gsl.Compilers.Create_Compiler;
      return Individual;
   end Create_Interpreter;

   ---------------------------------------------------------------------------
   --
   procedure Execute_Script
     (Individual : Interpreter;
      Name       : String) is
   begin
      Individual.Execution_Stack :=
        Giant.Gsl.Compilers.Get_Execution_Stack
          (Individual.Gsl_Compiler, Name);
      -- Individual.Result_Stack := Create
   end Execute_Script;

   ---------------------------------------------------------------------------
   --
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Giant.Evolutions.Evolution_Action) is
   begin
      Next_Action := Giant.Evolutions.Run; 
   end Step;

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean) is
   begin
      null;
   end Finish;

end Giant.Gsl.Interpreters;
