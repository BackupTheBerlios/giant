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
-- $Date: 2003/06/16 15:01:34 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Tags;
use  Ada.Tags;
with Ada.Strings.Unbounded;

with Giant.Default_Logger;
with Giant.Gsl.Types;
use  Giant.Gsl.Types;
with Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Interpreters is

   ---------------------------------------------------------------------------
   -- creates a new Gsl Interpreter
   -- only the Gsl_Compiler is initialized, all other fields are set to null
   function Create_Interpreter return Interpreter is

      Individual : Interpreter;
   begin
      Individual := new Interpreter_Record;
      Individual.Main_Activation_Record := null;
      Individual.Current_Activation_Record := null;
      Individual.Gsl_Compiler := Giant.Gsl.Compilers.Create_Compiler;
      return Individual;
   end Create_Interpreter;

   --------------------------------------------------------------------------
   -- destroys a Gsl Interpreter
   procedure Destroy
     (Gsl_Interpreter : Interpreter) is
   begin
      null;
   end Destroy;

   ---------------------------------------------------------------------------
   -- initilizes the Gsl Interpreter for Evolution.
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
        ("Interpreter: Initilize activation records.", "Giant.Gsl");
      Individual.Main_Activation_Record := Create_Activation_Record (null);
      Individual.Current_Activation_Record := 
        Individual.Main_Activation_Record;
      
      Default_Logger.Debug
        ("Interpreter: Register runtime functions.", "Giant.Gsl");
      Register_Runtime (Individual, Runtime_Set'Access, "set");
      Register_Runtime (Individual, Runtime_If'Access, "if");
      Register_Runtime (Individual, Runtime_Loop'Access, "loop");

      Default_Logger.Debug
        ("Interpreter: Initilize Evolution.", "Giant.Gsl");
      Initialize (Individual, 0);
   end Execute_Script;

   ---------------------------------------------------------------------------
   -- this procedure is called during the evolution
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Giant.Evolutions.Evolution_Action) is

      Cmd      : Syntax_Node;
      Res1     : Gsl_Type;
      Res2     : Gsl_Type;
      Lit      : Gsl_Type;
      Res_List : Gsl_List;
   begin
      if Execution_Stacks.Is_Empty (Individual.Execution_Stack) then
         Next_Action := Giant.Evolutions.Finish;
      else
         Execution_Stacks.Pop (Individual.Execution_Stack, Cmd);
         Default_Logger.Debug
           ("Interpreter: Execute Command.", "Giant.Gsl");
         Log_Syntax_Node (Cmd);
         Default_Logger.Debug
           ("", "Giant.Gsl");

         -- execute a Gsl command
         case Giant.Gsl.Syntax_Tree.Get_Node_Type (Cmd) is
            when Literal =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               if Lit /= Gsl_Null then
                  Lit := Copy (Lit);
                  Result_Stacks.Push (Individual.Result_Stack, Lit);
               else
                  Result_Stacks.Push (Individual.Result_Stack, Gsl_Null);
               end if;

            when Visible_Var =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Lit := Get_Var (Individual,
                 Get_Ref_Name (Gsl_Var_Reference (Lit)));
               if Lit /= Gsl_Null then
                  Lit := Copy (Lit);
                  Result_Stacks.Push (Individual.Result_Stack, Lit);
               else
                  Result_Stacks.Push (Individual.Result_Stack, Gsl_Null);
               end if;

            when Var_Creation =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Create_Var
                 (Individual, Get_Ref_Name (Gsl_Var_Reference (Lit)));
               Result_Stacks.Push (Individual.Result_Stack, Lit);

            when List =>
               Res_List := Create_Gsl_List
                 (Giant.Gsl.Syntax_Tree.Get_Size (Cmd));
               Result_Stacks.Push 
                 (Individual.Result_Stack, Gsl_Type (Res_List));
 
            when Script_Activation =>
               Result_Stacks.Pop (Individual.Result_Stack, Res2);
               Result_Stacks.Pop (Individual.Result_Stack, Res1);
               if Res1 = Gsl_Null then
                Default_Logger.Debug
                    ("Interpreter: Gsl_Script_Reference expected but Gsl_Null.",
                     "Giant.Gsl");

               elsif Res1'Tag = Gsl_Script_Reference_Record'Tag then
                  Default_Logger.Debug
                    ("Interpreter: Gsl_Script_Reference found.", 
                     "Giant.Gsl");
               end if;

               if Res1'Tag = Gsl_Script_Reference_Record'Tag then
                  if Res2'Tag = Gsl_List_Record'Tag then
                     case Get_Script_Type (Gsl_Script_Reference (Res1)) is
                        when Gsl_Script =>
                           null;

                        when Gsl_Runtime =>
                           Result_Stacks.Push (Individual.Result_Stack,
                             Get_Gsl_Runtime (Gsl_Script_Reference (Res1)) 
                               (Gsl_List (Res2)));
                     end case;

                  else
                     Default_Logger.Debug
                       ("Interpreter: Gsl_List expected.",
                        "Giant.Gsl");
                     Next_Action := Giant.Evolutions.Finish;
                  end if;
               else
                  Default_Logger.Debug
                    ("Interpreter: Gsl_Script_Reference expected.", 
                     "Giant.Gsl");
                    Next_Action := Giant.Evolutions.Finish;
               end if;

            when others =>
               null;

         end case;
         
         Next_Action := Giant.Evolutions.Run;
      end if;

      -- handling of exceptions 
      -- leading to runtime errors of Gsl
      exception
         when Var_Already_Exists =>
            Default_Logger.Debug
              ("Interpreter: var already exists.", "Giant.Gsl");
            Next_Action := Giant.Evolutions.Finish;
   end Step;

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean) is
   begin
      null;
   end Finish;

   procedure Register_Runtime
     (Individual : access Interpreter_Record;
      Runtime    : Runtime_Function;
      Name       : String) is

      Script : Gsl_Script_Reference;
   begin
      Script := Create_Gsl_Script_Reference (Runtime);
      Create_Var (Individual, Name);
      Set_Var (Individual, Name, Gsl_Type (Script));
   end Register_Runtime;

-----------------------------------------------------------------------------
-- functions for Activation_Records

   ---------------------------------------------------------------------------
   -- creates a new Activation_Record with Parent as
   -- the parent Activation_Record
   function Create_Activation_Record
     (Parent : Activation_Record)
      return Activation_Record is

      AR : Activation_Record;
   begin
      AR := new Activation_Record_Record;
      AR.Parent := Parent;
      AR.Vars := Gsl_Var_Hashed_Mappings.Create;
      return AR;
   end Create_Activation_Record;

   ---------------------------------------------------------------------------
   --
   procedure Destroy_Activation_Record
     (AR : Activation_Record) is
   begin
      null;
   end Destroy_Activation_Record;

   --------------------------------------------------------------------------
   --
   procedure Create_Var
     (Gsl_Interpreter : access Interpreter_Record;
      Name            : String) is

      AR : Activation_Record;
   begin
      AR := Gsl_Interpreter.Current_Activation_Record;
      if Gsl_Var_Hashed_Mappings.Is_Bound
        (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name)) then
         -- variable already exists, raise Exception
         raise Var_Already_Exists;
      else
         Gsl_Var_Hashed_Mappings.Bind
           (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name), 
            Gsl_Null);
      end if;
   end Create_Var;

   ---------------------------------------------------------------------------
   --
   function Get_Var
     (Gsl_Interpreter : access Interpreter_Record;
      Name            : String)
      return Gsl_Type is

      AR : Activation_Record;
   begin
      -- start in the current Activation_Record
      AR := Gsl_Interpreter.Current_Activation_Record;
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound
           (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name)) then
            return Gsl_Var_Hashed_Mappings.Fetch
              (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name));
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
      end loop;
      -- variable was not found, raise Exception
      raise Var_Not_Found;
   end Get_Var;

   ---------------------------------------------------------------------------
   --
   procedure Set_Var
     (Gsl_Interpreter : access Interpreter_Record;
      Name            : String; 
      Value           : Gsl_Type) is

      AR : Activation_Record;
   begin
      -- start in the current Activation_Record
      AR := Gsl_Interpreter.Current_Activation_Record;
      while AR /= null loop
         if Gsl_Var_Hashed_Mappings.Is_Bound 
           (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name)) then
            Gsl_Var_Hashed_Mappings.Unbind
              (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name));
            Gsl_Var_Hashed_Mappings.Bind
              (AR.Vars, Ada.Strings.Unbounded.To_Unbounded_String (Name), 
               Value);
            -- success, leave procedure here
            return;
         end if;
         -- next iteration look in the parent Activation_Record
         AR := AR.Parent;
      end loop;
      -- variable was not found, raise Exception
      raise Var_Not_Found;
   end Set_Var;

-----------------------------------------------------------------------------
-- some basic Runtime Functions

   function Runtime_Set
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      Default_Logger.Debug ("Interpreter: Runtime_Set called.", "Giant.Gsl");
      return Gsl_Null;
   end Runtime_Set;

   function Runtime_If
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      Default_Logger.Debug ("Interpreter: Runtime_If called.", "Giant.Gsl");
      return Gsl_Null;
   end Runtime_If;

   function Runtime_Loop
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      Default_Logger.Debug ("Interpreter: Runtime_Loop called.", "Giant.Gsl");
      return Gsl_Null;
   end Runtime_Loop;

end Giant.Gsl.Interpreters;
