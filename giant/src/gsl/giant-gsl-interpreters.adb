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
-- $Date: 2003/06/23 14:29:24 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Tags;
use  Ada.Tags;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

with Giant.Default_Logger;
with Giant.Gsl.Types;
use  Giant.Gsl.Types;
with Giant.Gsl.Syntax_Tree;
with Giant.Gsl.Runtime;

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
      Current_Interpreter := Individual;
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
      Register_Runtime (Runtime_Set'Access, "set");
      Register_Runtime (Runtime_If'Access, "if");
      Register_Runtime (Runtime_Loop'Access, "loop");

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
      if Execution_Stacks.Is_Empty (Current_Interpreter.Execution_Stack) then
         Next_Action := Giant.Evolutions.Finish;
      else
         Execution_Stacks.Pop (Current_Interpreter.Execution_Stack, Cmd);

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
               Lit := Get_Var (Get_Ref_Name (Gsl_Var_Reference (Lit)));
               if Lit /= Gsl_Null then
                  Lit := Copy (Lit);
                  Result_Stacks.Push (Individual.Result_Stack, Lit);
               else
                  Result_Stacks.Push (Individual.Result_Stack, Gsl_Null);
               end if;

            when Visible_Ref =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Lit := Copy (Lit);
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Var_Creation =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Create_Var (Get_Ref_Name (Gsl_Var_Reference (Lit)));
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Global_Ref =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Lit := Copy (Lit);
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Script_Decl =>
               Lit := Giant.Gsl.Syntax_Tree.Get_Literal (Cmd);
               Lit := Copy (Lit);
               Set_Activation_Record (Gsl_Script_Reference (Lit),
                 Create_Activation_Record
                   (Current_Interpreter.Current_Activation_Record));
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when List =>
               Res_List := Create_Gsl_List
                 (Giant.Gsl.Syntax_Tree.Get_Size (Cmd));
               for i in reverse 1 .. Get_List_Size (Res_List) loop
                  Result_Stacks.Pop (Individual.Result_Stack, Res1);
                  Set_Value_At (Res_List, i, Res1);
               end loop;
               Result_Stacks.Push 
                 (Individual.Result_Stack, Gsl_Type (Res_List));

            when Sequence =>
               if Giant.Gsl.Syntax_Tree.Get_Size (Cmd) = 0 then
                  Result_Stacks.Push
                    (Individual.Result_Stack, Gsl_Null);
               else
                  Result_Stacks.Pop (Individual.Result_Stack, Res1);
                  for i in 1 .. Giant.Gsl.Syntax_Tree.Get_Size (Cmd)-1 loop
                     Result_Stacks.Pop (Individual.Result_Stack, Res2);
                  end loop;
                  Result_Stacks.Push
                    (Individual.Result_Stack, Res1);
               end if;
 
            when Script_Activation => Script_Activation_Cmd;

            when Script_Exec =>
               Default_Logger.Debug
                 ("Interpreter: SCRIPT_EXEC.", "Giant.Gsl");
               Script_Exec_Cmd;

            when AR_Destroy =>
               Current_Interpreter.Current_Activation_Record :=
                 Current_Interpreter.Current_Activation_Record.Parent;

            when others =>
               null;

         end case;
         
         Next_Action := Giant.Evolutions.Run;
         Log_Result_Stack;
      end if;

      -- handling of exceptions 
      -- leading to runtime errors of Gsl
      exception
         when Var_Already_Exists =>
            Default_Logger.Debug
              ("Interpreter: var already exists.", "Giant.Gsl");
            Next_Action := Giant.Evolutions.Finish;
   end Step;

   --------------------------------------------------------------------------
   -- step 1 of a Gsl Script execution
   procedure Script_Activation_Cmd is

      Script : Gsl_Type;
      Params : Gsl_Type;
   begin
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Params);
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Script);
      if (Script = Gsl_Null) or (Params = Gsl_Null) then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Runtime Error!");
      elsif Script'Tag = Gsl_Script_Reference_Record'Tag then
         if Params'Tag = Gsl_List_Record'Tag then
            case Get_Script_Type (Gsl_Script_Reference (Script)) is
               when Gsl_Script =>
                  Default_Logger.Debug
                    ("Interpreter: --- GSL Script ---", "Giant.Gsl");
                  -- set the new Activation Record 
                  Current_Interpreter.Current_Activation_Record :=
                    Get_Activation_Record (Gsl_Script_Reference (Script));

                  Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                    Giant.Gsl.Compilers.Get_Execution_Stack
                      (Current_Interpreter.Gsl_Compiler,
                       Giant.Gsl.Syntax_Tree.Create_Node (Script_Exec, 
                         Null_Node, Null_Node))); 

                  -- push the code for the parameter list to the
                  -- Execution Stack
                  -- (get the Syntax_Node from the Gsl_Script_Reference)
                  Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                    Giant.Gsl.Compilers.Get_Execution_Stack
                      (Current_Interpreter.Gsl_Compiler, 
                       Get_Parameter_List (Gsl_Script_Reference (Script))));

                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Script);
                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Params);

               when Gsl_Runtime =>
                  Result_Stacks.Push (Current_Interpreter.Result_Stack,
                  Get_Gsl_Runtime (Gsl_Script_Reference (Script))
                    (Gsl_List (Params)));
               end case;
         else
            Ada.Exceptions.Raise_Exception
              (Gsl_Runtime_Error'Identity, "Gsl_List expected!");
         end if;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Gsl_Script_Reference expected!");
      end if;
   end Script_Activation_Cmd;

   --------------------------------------------------------------------------
   -- step 2 of a Gsl Script execution
   procedure Script_Exec_Cmd is 

      Script : Gsl_Type;
      Params : Gsl_Type;
      Formal : Gsl_Type;
      Ref    : Gsl_Type;
   begin
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Formal);
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Params);
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Script);
      if Get_List_Size (Gsl_List (Formal)) /= 
         Get_List_size (Gsl_List (Params)) then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Wrong number of parameters.");
      else
         for i in 1 .. Get_List_Size (Gsl_List (Formal)) loop
            Ref := Get_Value_At (Gsl_List (Formal), i);
            if Ref'Tag = Gsl_Var_Reference_Record'Tag then
               Set_Var (Get_Ref_Name (Gsl_Var_Reference (Ref)),
                  Get_Value_At (Gsl_List (Params), i));
             else
                Ada.Exceptions.Raise_Exception
                  (Gsl_Runtime_Error'Identity, "Gsl_Var_Reference expected.");
             end if; 
         end loop;
         -- destroy Activation Record when Script completed
         Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
           Giant.Gsl.Compilers.Get_Execution_Stack
             (Current_Interpreter.Gsl_Compiler,
              Giant.Gsl.Syntax_Tree.Create_Node (AR_Destroy, 
                Null_Node, Null_Node))); 

         -- push the code of the script
         Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
           Giant.Gsl.Compilers.Get_Execution_Stack
             (Current_Interpreter.Gsl_Compiler, 
              Get_Script_Node (Gsl_Script_Reference (Script))));
      end if;
      --Result_Stacks.Push (Current_Interpreter.Result_Stack, Gsl_Null);
      
   end Script_Exec_Cmd;

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean) is
   begin
      null;
   end Finish;

   ---------------------------------------------------------------------------
   --
   procedure Register_Runtime
      (Runtime    : Runtime_Function;
      Name       : String) is

      Script : Gsl_Script_Reference;
   begin
      Script := Create_Gsl_Script_Reference (Runtime);
      Create_Var (Name);
      Set_Var (Name, Gsl_Type (Script));
   end Register_Runtime;

   ---------------------------------------------------------------------------
   --
   procedure Log_Result_Stack is

      S : Result_Stacks.Stack;
      E : Gsl_Type;
   begin
      S := Result_Stacks.Copy (Current_Interpreter.Result_Stack);
      Default_Logger.Debug ("", "Giant.Gsl");
      Default_Logger.Debug ("-- Result Stack --", "Giant.Gsl");
      while Result_Stacks.Is_Empty (S) = false loop
         Result_Stacks.Pop (S, E);
         Default_Logger.Debug 
           ("Result Stack: " & Log_Gsl_Type (E), "Giant.Gsl");
      end loop;
   end Log_Result_Stack;

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
     (Name            : String) is

      AR : Activation_Record;
   begin
      AR := Current_Interpreter.Current_Activation_Record;
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
     (Name            : String)
      return Gsl_Type is

      AR : Activation_Record;
   begin
      -- start in the current Activation_Record
      AR := Current_Interpreter.Current_Activation_Record;
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
     (Name            : String; 
      Value           : Gsl_Type) is

      AR : Activation_Record;
   begin
      -- start in the current Activation_Record
      AR := Current_Interpreter.Current_Activation_Record;
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

   --------------------------------------------------------------------------
   -- 
   function Runtime_Set
     (Parameter : Gsl_List)
      return Gsl_Type is
   
      Var   : Gsl_Type;
      Value : Gsl_Type;
   begin
      Default_Logger.Debug ("Interpreter: Runtime_Set called.", "Giant.Gsl");
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'set' requires " &
             "2 Parameters");
      end if;
      Var := Get_Value_At (Parameter, 1);
      if Var'Tag /= Gsl_Var_Reference_Record'Tag then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Gsl_Var_Reference expected.");
      else
         Value := Get_Value_At (Parameter, 2);
         Set_Var (Get_Ref_Name (Gsl_Var_Reference (Var)), Value);
      end if;

      return Gsl_Null;
   end Runtime_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_If
     (Parameter : Gsl_List)
      return Gsl_Type is

      Cond         : Gsl_Type;
      True_Branch  : Gsl_Type;
      False_Branch : Gsl_Type;
      Param        : Gsl_List;
   begin
      Default_Logger.Debug ("Interpreter: Runtime_If called.", "Giant.Gsl");
      if Get_List_Size (Parameter) /= 3 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'if' requires " &
             "3 Parameters");
      end if;
      Cond := Get_Value_At (Parameter, 1);
      True_Branch := Get_Value_At (Parameter, 2);
      False_Branch := Get_Value_At (Parameter, 3);

      if Cond'Tag = Gsl_Script_Reference_Record'Tag then
         return Gsl_Null;
      elsif Cond'Tag = Gsl_Boolean_Record'Tag then
         if Get_Value (Gsl_Boolean (Cond)) = true then
            if True_Branch'Tag = Gsl_Script_Reference_Record'Tag then

               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                 Giant.Gsl.Compilers.Get_Execution_Stack
                   (Current_Interpreter.Gsl_Compiler,
                    Giant.Gsl.Syntax_Tree.Create_Node (Script_Activation, 
                    Null_Node, Null_Node))); 

               Result_Stacks.Push
                 (Current_Interpreter.Result_Stack, True_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return True_Branch;
            end if;
         else
            if False_Branch'Tag = Gsl_Script_Reference_Record'Tag then

               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                 Giant.Gsl.Compilers.Get_Execution_Stack
                   (Current_Interpreter.Gsl_Compiler,
                    Giant.Gsl.Syntax_Tree.Create_Node (Script_Activation, 
                    Null_Node, Null_Node))); 

               Result_Stacks.Push
                 (Current_Interpreter.Result_Stack, True_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return False_Branch;
            end if;
         end if;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Gsl_Script_Reference or " &
             "Gsl_Boolean expected.");
      end if;
   end Runtime_If;

   ---------------------------------------------------------------------------
   --
   function Runtime_Loop
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      Default_Logger.Debug ("Interpreter: Runtime_Loop called.", "Giant.Gsl");
      return Gsl_Null;
   end Runtime_Loop;

   ---------------------------------------------------------------------------
   --
   function Runtime_Error
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      Ada.Exceptions.Raise_Exception
        (Gsl_Runtime_Error'Identity, "Runtime Error.");
      return Gsl_Null;
   end Runtime_Error;

end Giant.Gsl.Interpreters;
