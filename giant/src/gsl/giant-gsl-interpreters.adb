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
-- $Date: 2003/06/09 14:23:35 $
--
-- This package implements the datatypes used in GSL.
--

with Giant.Default_Logger;

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
      Default_Logger.Debug
        ("Interpreter: Call compiler.", "Giant.Gsl");
      Individual.Execution_Stack :=
        Giant.Gsl.Compilers.Get_Execution_Stack
          (Individual.Gsl_Compiler, Name);
      Default_Logger.Debug
        ("Interpreter: Initilize result stack.", "Giant.Gsl");
      Individual.Result_Stack := Result_Stacks.Create;
      Default_Logger.Debug
        ("Interpreter: Initilize Evolution Object.", "Giant.Gsl");
      Initialize (Individual, 0);
   end Execute_Script;

   ---------------------------------------------------------------------------
   --
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Giant.Evolutions.Evolution_Action) is

      Cmd : Syntax_Node;
   begin
      if Execution_Stacks.Is_Empty (Individual.Execution_Stack) then
         Next_Action := Giant.Evolutions.Finish;
      else
         Execution_Stacks.Pop (Individual.Execution_Stack, Cmd);
         Log_Syntax_Node (Cmd);
         Next_Action := Giant.Evolutions.Run;
      end if;
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
