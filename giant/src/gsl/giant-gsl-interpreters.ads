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
-- $RCSfile: giant-gsl-interpreters.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/06/30 16:03:29 $
--
-- This package implements the Gsl interpreter.
--

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Giant.Evolutions;
with Giant.Gsl.Compilers;
with Giant.Gsl.Types;
use  Giant.Gsl.Types;

package Giant.Gsl.Interpreters is

   ---------------------------------------------------------------------------
   -- the GSL interpreter, inherits Iterative_Evolution 
   type Interpreter_Record is new Giant.Evolutions.Iterative_Evolution
     with private;

   type Interpreter is access all Interpreter_Record'Class;

   --------------------------------------------------------------------------
   --
   function Create_Interpreter return Interpreter;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Interpreter return Interpreter;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Execution_Stack return Execution_Stacks.Stack;

   ---------------------------------------------------------------------------
   --
   function Get_Current_Result_Stack return Result_Stacks.Stack;
  
   ---------------------------------------------------------------------------
   --
   function Get_Current_Compiler return Giant.Gsl.Compilers.Compiler;

   ---------------------------------------------------------------------------
   --
   procedure Execute_Script
     (Individual : Interpreter;
      Name       : String;
      Context    : String);

   ---------------------------------------------------------------------------
   --
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Giant.Evolutions.Evolution_Action);

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean); 

   ---------------------------------------------------------------------------
   --
   procedure Register_Runtime
     (Runtime    : Runtime_Function;
      Name       : String);

   procedure Log_Result_Stack;

   ---------------------------------------------------------------------------
   --
   procedure Create_Var
     (Name            : String);

   ---------------------------------------------------------------------------
   --
   function Get_Var
     (Name            : String)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   procedure Set_Var
     (Name            : String;
      Value           : Gsl_Type);

private

   ---------------------------------------------------------------------------
   --
   Current_Interpreter : Interpreter;

   ---------------------------------------------------------------------------
   --
   procedure Script_Activation_Cmd;
   
   ---------------------------------------------------------------------------
   --
   procedure Script_Exec_Cmd;

   ---------------------------------------------------------------------------
   --
   function Create_Activation_Record
     (Parent : Activation_Record)
      return Activation_Record;
   
   ---------------------------------------------------------------------------
   --
   procedure Destroy_Activation_Record
     (AR : Activation_Record);

   ---------------------------------------------------------------------------
   -- the GSL interpreter, inherits Iterative_Evolution 
   type Interpreter_Record is new Giant.Evolutions.Iterative_Evolution with
      record
         Context                   : Unbounded_String;
         Script                    : Unbounded_String;
         Execution_Stack           : Execution_Stacks.Stack;
         Result_Stack              : Result_Stacks.Stack;
         Main_Activation_Record    : Activation_Record;
         Current_Activation_Record : Activation_Record;
         Gsl_Compiler              : Giant.Gsl.Compilers.Compiler;
      end record;

end Giant.Gsl.Interpreters;
