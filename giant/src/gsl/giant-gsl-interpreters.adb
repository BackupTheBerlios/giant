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
-- $Date: 2003/07/07 16:18:14 $
--
-- This package implements the datatypes used in GSL.
--

with Text_IO;

with Ada.Tags;
use  Ada.Tags;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

with Giant.Controller;
with Giant.Default_Logger;
with Giant.Graph_Lib.Subgraphs;
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

   -------------------------------------------------------------------------
   --
   function Get_Current_Interpreter return Interpreter is
   begin
      return Current_Interpreter;
   end Get_Current_Interpreter;

   -------------------------------------------------------------------------
   --
   function Get_Current_Execution_Stack return Execution_Stacks.Stack is
   begin
      return Current_Interpreter.Execution_Stack;
   end Get_Current_Execution_Stack;

   -------------------------------------------------------------------------
   --
   function Get_Current_Result_Stack return Result_Stacks.Stack is
   begin
      return Current_Interpreter.Result_Stack;
   end Get_Current_Result_Stack;

   -------------------------------------------------------------------------
   --
   function Get_Current_Compiler return Giant.Gsl.Compilers.Compiler is
   begin
      return Current_Interpreter.Gsl_Compiler;
   end Get_Current_Compiler;

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
      Name       : String;
      Context    : String) is
   begin
      -- set the current interpreter and initilize context and script
      Current_Interpreter := Individual;
      Individual.Script := To_Unbounded_String (Name);
      Individual.Context := To_Unbounded_String (Context);
      -- initialize the execution stack with the
      Individual.Execution_Stack := Giant.Gsl.Compilers.Get_Execution_Stack
        (Individual.Gsl_Compiler, Name);
      -- initialize the result stack
      Individual.Result_Stack := Result_Stacks.Create;
      -- initialize the stack for all activation records
      Individual.Activation_Records := Activation_Record_Stacks.Create;
      -- create the main activation record and set it acitve
      Individual.Main_Activation_Record := Create_Activation_Record (null);
      Individual.Current_Activation_Record :=
        Individual.Main_Activation_Record;
      -- register runtime functions
      Default_Logger.Debug
        ("Interpreter: Register runtime functions.", "Giant.Gsl");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Set'Access, "set");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_If'Access, "if");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Loop'Access, "loop");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Error'Access, "error");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Run'Access, "run");

      -- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Add'Access, "add");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Sub'Access, "sub");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Cat'Access, "cat");

      -- compare (ref. GIANT Scripting Language Specification 1.5.1.4)
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Less'Access, "less");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Equal'Access, "equal");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_In_Regexp'Access,
                        "in_regexp");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Type_In'Access, "type_in");

      -- types (ref. GIANT Scripting Language Specification 1.5.1.6)
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Is_Nodeid'Access,
                        "is_nodeid");
      Register_Runtime (Giant.Gsl.Runtime.Runtime_Is_Edgeid'Access,
                        "is_edgeid");

      -- initilze the evolution object, comlexity is 0
      Initialize (Individual, 0);
   end Execute_Script;

   ---------------------------------------------------------------------------
   -- this procedure is called during the evolution
   procedure Step
     (Individual  : access Interpreter_Record;
      Next_Action : out    Giant.Evolutions.Evolution_Action) is

      use Giant.Gsl.Syntax_Tree;
      use Giant.Controller;

      Cmd      : Syntax_Node;
      Res1     : Gsl_Type;
      Res2     : Gsl_Type;
      Lit      : Gsl_Type;
      Res_List : Gsl_List;
      Sub      : Giant.Graph_Lib.Subgraphs.Subgraph;
   begin
      if Execution_Stacks.Is_Empty (Current_Interpreter.Execution_Stack) then
         Next_Action := Giant.Evolutions.Finish;
      elsif Interpreter (Individual) /= Current_Interpreter then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
           "Gsl Interpreter: Invalid interpreter.");
      else
         Execution_Stacks.Pop (Current_Interpreter.Execution_Stack, Cmd);

         -- execute a Gsl command
         case Get_Node_Type (Cmd) is
            when Literal =>
               Lit := Get_Literal (Cmd);
               if Lit /= Gsl_Null then
                  Lit := Copy (Lit);
                  Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);
               else
                  Result_Stacks.Push (Current_Interpreter.Result_Stack, 
                                      Gsl_Null);
               end if;

            when Visible_Var =>
               Lit := Get_Literal (Cmd);
               -- get value from activation record
               Lit := Get_Var (Get_Ref_Name (Gsl_Var_Reference (Lit)));
               if Lit /= Gsl_Null then
                  Lit := Copy (Lit);
                  Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);
               else
                  Result_Stacks.Push (Current_Interpreter.Result_Stack,
                                      Gsl_Null);
               end if;

            when Global_Var =>
               -- controller
               Lit := Get_Literal (Cmd);
               if Get_Ref_Type (Gsl_Var_Reference (Lit)) = Subgraph then
                  if Exists_Subgraph (Get_Ref_Name
                       (Gsl_Var_Reference (Lit))) then
                     Sub := Get_Subgraph (Get_Ref_Name (Gsl_Var_Reference (Lit)));
                     Res_List := Create_Gsl_List (2);
                     Set_Value_At (Res_List, 1, Gsl_Type (Create_Gsl_Node_Set
                       (Giant.Graph_Lib.Subgraphs.Get_All_Nodes (Sub))));
                     Set_Value_At (Res_List, 2, Gsl_Type (Create_Gsl_Edge_Set
                       (Giant.Graph_Lib.Subgraphs.Get_All_Edges (Sub))));
                     Result_Stacks.Push (Current_Interpreter.Result_Stack, 
                       Gsl_Type (Res_List));
                  else
                     Ada.Exceptions.Raise_Exception
                       (Gsl_Runtime_Error'Identity, 
                        "Gsl Interpreter: Subgraph does not exist.");
                  end if;
               elsif Get_Ref_Type (Gsl_Var_Reference (Lit)) = Selection then
                  null;
                  --Giant.Controller.Get_Selection ();
               end if;

            when Visible_Ref =>
               Lit := Get_Literal (Cmd);
               Lit := Copy (Lit);
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Var_Creation =>
               Lit := Get_Literal (Cmd);
               Create_Var (Get_Ref_Name (Gsl_Var_Reference (Lit)));
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Global_Ref =>
               Lit := Get_Literal (Cmd);
               Lit := Copy (Lit);
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when Script_Decl =>
               Lit := Get_Literal (Cmd);
               Lit := Copy (Lit);
               Set_Activation_Record (Gsl_Script_Reference (Lit),
                 Current_Interpreter.Current_Activation_Record);
               Result_Stacks.Push (Current_Interpreter.Result_Stack, Lit);

            when List =>
               Res_List := Create_Gsl_List (Get_Size (Cmd));
               for i in reverse 1 .. Get_List_Size (Res_List) loop
                  Result_Stacks.Pop (Current_Interpreter.Result_Stack, Res1);
                  Set_Value_At (Res_List, i, Res1);
               end loop;
               Result_Stacks.Push
                 (Current_Interpreter.Result_Stack, Gsl_Type (Res_List));

            when Sequence =>
               if Giant.Gsl.Syntax_Tree.Get_Size (Cmd) = 0 then
                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Gsl_Null);
               else
                  Result_Stacks.Pop (Current_Interpreter.Result_Stack, Res1);
                  for i in 1 .. Get_Size (Cmd)-1 loop
                     Result_Stacks.Pop
                       (Current_Interpreter.Result_Stack, Res2);
                     -- free the memory
                     Destroy_Gsl_Type (Res2);
                  end loop;
                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Res1);
               end if;

            when Script_Activation => Script_Activation_Cmd;

            when Script_Exec => Script_Exec_Cmd;

            when Script_Finish =>
               Default_Logger.Debug
                 ("Interpreter: DESTROY Activation_Record.", "Giant.Gsl");
               Destroy_Activation_Record
                 (Current_Interpreter.Current_Activation_Record);
               Activation_Record_Stacks.Pop
                 (Current_Interpreter.Activation_Records,
                  Current_Interpreter.Current_Activation_Record);

            when Script_Loop =>
               Result_Stacks.Pop (Current_Interpreter.Result_Stack, Res1);
               if Res1 = Gsl_Null then
                  Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
                    "Runtime Error - Gsl_Null!");
               elsif Res1'Tag = Gsl_Boolean_Record'Tag then
                  Default_Logger.Debug
                    ("Interpreter: Loop correct.", "Giant.Gsl");
                  if Get_Value (Gsl_Boolean (Res1)) then
                  -- loop again
                     Execution_Stacks.Push
                       (Current_Interpreter.Execution_Stack,
                        Copy_Node (Cmd));
                  -- push the code of the script
                     Execution_Stacks.Push
                       (Current_Interpreter.Execution_Stack,
                        Giant.Gsl.Compilers.Get_Execution_Stack
                          (Current_Interpreter.Gsl_Compiler,
                           Get_Child1 (Cmd)));
                  else
                     null;
                  end if;
               else
                  Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
                    "Runtime Error - Gsl_Boolean expected!");
               end if;

            when Result_Pop =>
               Result_Stacks.Pop (Current_Interpreter.Result_Stack, Res1);
               Destroy_Gsl_Type (Res1);

            when others => null;
         end case;

         --  advance
         Evolutions.Advance_Progress (Individual, 1);

         -- destroy the command, free memory
         Giant.Gsl.Syntax_Tree.Destroy_Node (Cmd);
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

      use Giant.Gsl.Compilers;

      Script : Gsl_Type;
      Params : Gsl_Type;
   begin
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Params);
      Result_Stacks.Pop (Current_Interpreter.Result_Stack, Script);
      if (Script = Gsl_Null) or (Params = Gsl_Null) then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
           "Runtime Error!");
      elsif Script'Tag = Gsl_Script_Reference_Record'Tag then
         if Params'Tag = Gsl_List_Record'Tag then
            case Get_Script_Type (Gsl_Script_Reference (Script)) is
               when Giant.Gsl.Types.Gsl_Script =>
                  -- set the new activation record and push the old one
                  -- to the activation record stack
                  Set_Activation_Record (Create_Activation_Record
                    (Get_Activation_Record (Gsl_Script_Reference (Script))));

                  Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                    Get_Execution_Stack (Current_Interpreter.Gsl_Compiler,
                       Giant.Gsl.Syntax_Tree.Create_Node (Script_Exec,
                         Null_Node, Null_Node)));

                  -- push the code for the parameter list to the
                  -- execution stack
                  -- (get the Syntax_Node from the Gsl_Script_Reference)
                  Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
                    Get_Execution_Stack (Current_Interpreter.Gsl_Compiler,
                       Get_Parameter_List (Gsl_Script_Reference (Script))));

                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Script);
                  Result_Stacks.Push
                    (Current_Interpreter.Result_Stack, Params);

               when Giant.Gsl.Types.Gsl_Runtime =>
                  -- call runtime function, and push the result
                  Result_Stacks.Push (Current_Interpreter.Result_Stack,
                    Get_Gsl_Runtime (Gsl_Script_Reference (Script))
                                       (Gsl_List (Params)));
                  -- free the parameter
                  -- Destroy_Gsl_Type (Params);
               end case;
         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
              "Gsl_List expected.");
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Gsl_Script_Reference expected.");
      end if;
   end Script_Activation_Cmd;

   --------------------------------------------------------------------------
   -- step 2 of a Gsl Script execution
   procedure Script_Exec_Cmd is

      use Giant.Gsl.Compilers;

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
         -- process the parameter list
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
         -- free the parameter
         -- Destroy_Gsl_Type (Params); 
         -- destroy Activation Record when Script completed
         Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
           Get_Execution_Stack (Current_Interpreter.Gsl_Compiler,
             Giant.Gsl.Syntax_Tree.Create_Node (Script_Finish,
                                                Null_Node, Null_Node)));

         -- push the code of the script
         Execution_Stacks.Push (Current_Interpreter.Execution_Stack,
           Get_Execution_Stack (Current_Interpreter.Gsl_Compiler,
             Get_Script_Node (Gsl_Script_Reference (Script))));
      end if;
   end Script_Exec_Cmd;

   ---------------------------------------------------------------------------
   --
   procedure Finish
     (Individual : access Interpreter_Record;
      Canceled   : in     Boolean) is
   begin
      Destroy_Activation_Record (Individual.Main_Activation_Record);
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

------------------------------------------------------------------------------
-- functions for Activation_Records

   ---------------------------------------------------------------------------
   -- sets the current activation record
   procedure Set_Activation_Record
     (AR : Activation_Record) is
   begin
      -- save the current activation record on the stack
      Activation_Record_Stacks.Push
        (Current_Interpreter.Activation_Records,
         Current_Interpreter.Current_Activation_Record);
      -- set the current activation record
      Current_Interpreter.Current_Activation_Record := AR;
   end Set_Activation_Record;

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

      Iter : Gsl_Var_Hashed_Mappings.Values_Iter;
      Var  : Gsl_Type;
   begin
      Iter := Gsl_Var_Hashed_Mappings.Make_Values_Iter (AR.Vars);
      while Gsl_Var_Hashed_Mappings.More (Iter) loop
         Gsl_Var_Hashed_Mappings.Next (Iter, Var);
         Destroy_Gsl_Type (Var);
      end loop;
      Gsl_Var_Hashed_Mappings.Destroy (AR.Vars);
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

end Giant.Gsl.Interpreters;
