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
-- $RCSfile: giant-gsl-runtime.adb,v $
-- $Author: schulzgt $
-- $Date: 2003/07/24 14:42:46 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Exceptions;

with GNAT.Regpat;

with Giant.Controller;

with Giant.Graph_Lib;
use type Giant.Graph_Lib.Edge_Id;
use type Giant.Graph_Lib.Node_Id;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Lib.Selections;

with Giant.GSL_Support;

------------------------------------------------------------------------------
-- Gsl Includes
with Giant.Gsl.Compilers;
with Giant.Gsl.Interpreters;
with Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Runtime is

   --------------------------------------------------------------------------
   --
   function Runtime_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Graph_Lib.Subgraphs;
      use Giant.Graph_Lib.Selections;
      use Giant.Gsl.Interpreters;
      Var   : Gsl_Type;
      Value : Gsl_Type;
      Sub   : Giant.Graph_Lib.Subgraphs.Subgraph;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
           "Script 'set': Expecting 2 parameters.");
      end if;
      Var := Get_Value_At (Parameter, 1);
      Value := Get_Value_At (Parameter, 2);
      if not (Is_Gsl_Var_Reference (Var) or Is_Gsl_Global_Reference (Var))
      then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set': Gsl_Var_Reference expected.");
      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Var then
         Set_Var (Get_Ref_Name (Gsl_Var_Reference (Var)), Value);
      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Subgraph then
         if Is_Gsl_Object_Set (Value) then
            Sub := Create (Get_Ref_Name (Gsl_Var_Reference (Var)));
            Add_Node_Set (Sub, Get_Value (Gsl_Node_Set 
              (Get_Value_At (Gsl_List (Value), 1))));
            Add_Edge_Set (Sub, Get_Value (Gsl_Edge_Set
              (Get_Value_At (Gsl_List (Value), 2))));
            Giant.Controller.Add_Subgraph (Sub);
         end if;

      elsif Get_Ref_Type (Gsl_Var_Reference (Var)) = Gsl.Types.Selection then
         null;
      end if;
      return Gsl_Null;
   end Runtime_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_If
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
      use Giant.Gsl.Compilers;
      use Giant.Gsl.Syntax_Tree;
      Cond         : Gsl_Type;
      True_Branch  : Gsl_Type;
      False_Branch : Gsl_Type;
      Param        : Gsl_List;
      Comp         : Compiler;
      ES           : Execution_Stacks.Stack;
      RS           : Result_Stacks.Stack;
   begin
      Comp := Get_Current_Compiler;
      ES := Get_Current_Execution_Stack;
      RS := Get_Current_Result_Stack;

      if Get_List_Size (Parameter) /= 3 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'if': Expecting 3 parameters.");
      end if;
      Cond := Get_Value_At (Parameter, 1);
      True_Branch := Get_Value_At (Parameter, 2);
      False_Branch := Get_Value_At (Parameter, 3);
      if Is_Gsl_Boolean (Cond) then
         if Get_Value (Gsl_Boolean (Cond)) = true then
            if Is_Gsl_Script_Reference (True_Branch) then
               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (ES, Get_Execution_Stack
                 (Comp, Create_Node (Script_Activation, 
                                     Null_Node, Null_Node)));

               Result_Stacks.Push (RS, True_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return True_Branch;
            end if;
         else
            if Is_Gsl_Script_Reference (False_Branch) then
               -- Script_Activation to Execution Stack
               Execution_Stacks.Push (ES, Get_Execution_Stack
                 (Comp, Create_Node (Script_Activation, 
                                     Null_Node, Null_Node)));

               Result_Stacks.Push (RS, True_Branch);
               Param := Create_Gsl_List (0);
               return Gsl_Type (Param);
            else
               return False_Branch;
            end if;
         end if;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'if': Gsl_Boolean expected.");
      end if;
   end Runtime_If;

   ---------------------------------------------------------------------------
   --
   function Runtime_Loop
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
      use Giant.Gsl.Compilers;
      use Giant.Gsl.Syntax_Tree;
      Script   : Gsl_Type;
      Loop_Cmd : Syntax_Node;
      Comp     : Compiler;
      ES       : Execution_Stacks.Stack;
   begin
      Comp := Get_Current_Compiler;
      ES := Get_Current_Execution_Stack;
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'loop': Expecting 1 parameter.");
      end if;
      Script := Get_Value_At (Parameter, 1);
      if Is_Gsl_Script_Reference (Script) then
         Loop_Cmd := Create_Node (Script_Loop, 
           Get_Script_Node (Gsl_Script_Reference (Script)), Null_Node);
         Execution_Stacks.Push (ES, Loop_cmd);

         -- set the new activation record
         Set_Activation_Record (Create_Activation_Record
           (Get_Activation_Record (Gsl_Script_Reference (Script))));

         -- push the code of the script
         Execution_Stacks.Push (ES, Get_Execution_Stack (Comp,
           Get_Script_Node (Gsl_Script_Reference (Script))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'loop': Gsl_Script_Reference expected.");
      end if; 
      return Gsl_Null;
   end Runtime_Loop;

   ---------------------------------------------------------------------------
   --
   function Runtime_Error
     (Parameter : Gsl_List)
      return Gsl_Type is

      Message : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
           "Script 'error': Expecting 1 parameter.");
      end if;
      Message := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Message) then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           Get_Value (Gsl_String (Message)));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity, 
           "Script 'error': Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Error;

   ---------------------------------------------------------------------------
   --
   function Runtime_Run
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
      use Giant.Gsl.Compilers;
      use Giant.Gsl.Syntax_Tree;
      Name : Gsl_Type;
      Comp : Compiler;
      ES   : Execution_Stacks.Stack;
   begin
      Comp := Get_Current_Compiler;
      ES := Get_Current_Execution_Stack;
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'run': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         -- push the code of the library to the execution stack 
         Execution_Stacks.Push (ES, Get_Execution_Stack (Comp, 
           Giant.GSL_Support.Get_GSL_Include (Get_Value (Gsl_String (Name)) 
                                                         & ".gsl")));
         -- remove the Gsl_Null result from the result stack in the next step
         Execution_Stacks.Push (ES, Get_Execution_Stack (Comp,
           Create_Node (Result_Pop, Null_Node, Null_Node)));
         return Gsl_Null;
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'run': Gsl_String expected.");
      end if;
   end Runtime_Run;

------------------------------------------------------------------------------
-- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)

   ---------------------------------------------------------------------------
   --
   function Runtime_Add
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
      A        : Gsl_Type;
      B        : Gsl_Type;
      Var      : Gsl_Type;
      Node_Set : Giant.Graph_Lib.Node_Id_Sets.Set;
      Edge_Set : Giant.Graph_Lib.Edge_Id_Sets.Set;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'add': Expecting 2 parameters.");
      end if;
      A := Get_Value_At (Parameter, 1);
      B := Get_Value_At (Parameter, 2);
      if Is_Gsl_Natural (A) and Is_Gsl_Natural (B) then
         -- Gsl_Natural, normal addition
         return Gsl_Type (Create_Gsl_Natural
           (Get_Value (Gsl_Natural (A)) + Get_Value (Gsl_Natural (B))));

      elsif Is_Gsl_Var_Reference (A) then
         if Is_Gsl_Node_Id (B) then
            Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (A)));
            if Is_Gsl_Node_Set (Var) then
               Node_Set := Get_Value (Gsl_Node_Set (Var));
               Giant.Graph_Lib.Node_Id_Sets.Insert
                 (Node_Set, Get_Value (Gsl_Node_Id (B)));
               Set_Value (Gsl_Node_Set (Var), Node_Set);
               return Gsl_Null;
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'add': Gsl_Node_Set expected.");
            end if;

         elsif Is_Gsl_Edge_Id (B) then
            Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (B)));
            if Is_Gsl_Edge_Set (Var) then
               Edge_Set := Get_Value (Gsl_Edge_Set (Var));
               Giant.Graph_Lib.Edge_Id_Sets.Insert
                 (Edge_Set, Get_Value (Gsl_Edge_Id (B)));
               Set_Value (Gsl_Edge_Set (Var), Edge_Set);
               return Gsl_Null;
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'add': Gsl_Edge_Set expected.");
            end if;

         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'add': Gsl_Node_Id or Gsl_Edge_Id expected.");
         end if;

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'add': Gsl_Natural or Gsl_Var_Reference expected.");
      end if;
   end Runtime_Add;

   ---------------------------------------------------------------------------
   --
   function Runtime_Sub
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
      A        : Gsl_Type;
      B        : Gsl_Type;
      Var      : Gsl_Type;
      Node_Set : Giant.Graph_Lib.Node_Id_Sets.Set;
      Edge_Set : Giant.Graph_Lib.Edge_Id_Sets.Set;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'sub': Expecting 2 parameters.");
      end if;
      A := Get_Value_At (Parameter, 1);
      B := Get_Value_At (Parameter, 2);
      if Is_Gsl_Natural (A) and Is_Gsl_Natural (B) then
         -- Gsl_Natural, normal substraction
         return Gsl_Type (Create_Gsl_Natural
           (Get_Value (Gsl_Natural (A)) - Get_Value (Gsl_Natural (B))));

      elsif Is_Gsl_Var_Reference (A) then
         if Is_Gsl_Node_Id (B) then
            Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (A)));
            if Is_Gsl_Node_Set (Var) then
               Node_Set := Get_Value (Gsl_Node_Set (Var));
               Giant.Graph_Lib.Node_Id_Sets.Remove_If_Exists
                 (Node_Set, Get_Value (Gsl_Node_Id (B)));
               Set_Value (Gsl_Node_Set (Var), Node_Set);
               return Gsl_Null;
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'sub': Gsl_Node_Set expected.");
            end if;

         elsif Is_Gsl_Edge_Id (B) then
            Var := Get_Var (Get_Ref_Name (Gsl_Var_Reference (B)));
            if Is_Gsl_Edge_Set (Var) then
               Edge_Set := Get_Value (Gsl_Edge_Set (Var));
               Giant.Graph_Lib.Edge_Id_Sets.Remove_If_Exists
                 (Edge_Set, Get_Value (Gsl_Edge_Id (B)));
               Set_Value (Gsl_Edge_Set (Var), Edge_Set);
               return Gsl_Null;
            else
               Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
                 "Script 'sub': Gsl_Edge_Set expected.");
            end if;

         else
            Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
              "Script 'sub': Gsl_Node_Id or Gsl_Edge_Id expected.");
         end if;

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'sub': Gsl_Natural or Gsl_Var_Reference expected.");
      end if;
   end Runtime_Sub;

   ---------------------------------------------------------------------------
   --
   function Runtime_Cat
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Cat    : Gsl_String;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'cat': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         Cat := Create_Gsl_String (Get_Value (Gsl_String (Param1)) & 
                                   Get_Value (Gsl_String (Param2)));
         return Gsl_Type (Cat);
      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'cat': Gsl_String expected.");
      end if;
   end Runtime_Cat;

------------------------------------------------------------------------------
-- compare (ref. GIANT Scripting Language Specification 1.5.1.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Less
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Res    : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'less': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_Natural (Param1) and Is_Gsl_Natural (Param2) then
         if Get_Value (Gsl_Natural (Param1)) <
            Get_Value (Gsl_Natural (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;
 
      elsif Is_Gsl_Edge_Id (Param1) and Is_Gsl_Edge_Id (Param2) then
         if Get_Value (Gsl_Edge_Id (Param1)) <
            Get_Value (Gsl_Edge_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Node_Id (Param1) and Is_Gsl_Node_Id (Param2) then
         if Get_Value (Gsl_Node_Id (Param1)) <
            Get_Value (Gsl_Node_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         if Get_Value (Gsl_String (Param1)) <
            Get_Value (Gsl_String (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'less': Gsl_Natural, " &
            "Gsl_Edge_Id, Gsl_Node_Id or Gsl_String expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Less;

   ---------------------------------------------------------------------------
   --
   function Runtime_Equal
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
      Res    : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'equal': Expecting 2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);

      if Is_Gsl_Natural (Param1) and Is_Gsl_Natural (Param2) then
         if Get_Value (Gsl_Natural (Param1)) =
            Get_Value (Gsl_Natural (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;
 
      elsif Is_Gsl_Edge_Id (Param1) and Is_Gsl_Edge_Id (Param2) then
         if Get_Value (Gsl_Edge_Id (Param1)) =
            Get_Value (Gsl_Edge_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_Node_Id (Param1) and Is_Gsl_Node_Id (Param2) then
         if Get_Value (Gsl_Node_Id (Param1)) =
            Get_Value (Gsl_Node_Id (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      elsif Is_Gsl_String (Param1) and Is_Gsl_String (Param2) then
         if Get_Value (Gsl_String (Param1)) =
            Get_Value (Gsl_String (Param2)) then
            Res := Create_Gsl_Boolean (true);
         else
            Res := Create_Gsl_Boolean (false);
         end if;

      else
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'equal': Gsl_Natural, " &
            "Gsl_Edge_Id, Gsl_Node_Id or Gsl_String expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Equal;

   ---------------------------------------------------------------------------
   --
   function Runtime_In_Regexp
     (Parameter : Gsl_List)
      return Gsl_Type is

      use GNAT.Regpat;

      Regexp  : Gsl_Type;
      Data    : Gsl_Type;
      Res     : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'in_regexp': Expecting 2 parameters.");
      end if;
      Data:= Get_Value_At (Parameter, 1);
      Regexp := Get_Value_At (Parameter, 2);

      if Is_Gsl_String (Data) and Is_Gsl_String (Regexp) then
         Res := Create_Gsl_Boolean (Match (Get_Value (Gsl_String (Regexp)), 
                                           Get_Value (Gsl_String (Data))));
         if Match (Get_Value (Gsl_String (Regexp)), 
                   Get_Value (Gsl_String (Data))) then
            Default_Logger.Debug ("RegExp matched");
         end if;
         return Gsl_Type (Res);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'in_regexp': Gsl_String expected.");
      end if;
   end Runtime_In_Regexp;

   ---------------------------------------------------------------------------
   --
   function Runtime_Type_In
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Type_In;

------------------------------------------------------------------------------
-- sets and lists (ref. GIANT Scripting Language Specification 1.5.1.5)

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Node_Set : Gsl_Node_Set;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'empty_node_set': Expecting no parameters.");
      end if;
      Node_Set := Create_Gsl_Node_Set
        (Giant.Graph_Lib.Node_Id_Sets.Empty_Set);
      return Gsl_Type (Node_Set);
   end Runtime_Empty_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Edge_Set : Gsl_Edge_Set;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'empty_edge_set': Expecting no parameters.");
      end if;
      Edge_Set := Create_Gsl_Edge_Set
        (Giant.Graph_Lib.Edge_Id_Sets.Empty_Set);
      return Gsl_Type (Edge_Set);
   end Runtime_Empty_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_In
     (Parameter : Gsl_List)
      return Gsl_Type is

      Set     : Gsl_Type;
      Element : Gsl_Type;
      Res     : Gsl_Boolean;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'is_in': Expecting 2 parameters.");
      end if;
      Set := Get_Value_At (Parameter, 1);
      Element := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Set (Set) and Is_Gsl_Node_Id (Element) then
         Res := Create_Gsl_Boolean (Giant.Graph_Lib.Node_Id_Sets.Is_Member
                                    (Get_Value (Gsl_Node_Set (Set)),
                                     Get_Value (Gsl_Node_Id (Element))));
      elsif Is_Gsl_Edge_Set (Set) and Is_Gsl_Edge_Id (Element) then
         Res := Create_Gsl_Boolean (Giant.Graph_Lib.Edge_Id_Sets.Is_Member
                                    (Get_Value (Gsl_Edge_Set (Set)),
                                     Get_Value (Gsl_Edge_Id (Element))));

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'is_in': Gsl_Node_Set or Gsl_Edge_Set and " &
           "Gsl_Node_Id or Gsl_Edge_Id expected.");
      end if;
      return Gsl_Type (Res);
   end Runtime_Is_In;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_First
     (Parameter : Gsl_List)
      return Gsl_Type is
      
      Obj  : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_Node_Set (Obj) then
         return Gsl_Type (Create_Gsl_Node_Id
           (Giant.Graph_Lib.Node_Id_Sets.First
             (Get_Value (Gsl_Node_Set (Obj)))));

      elsif Is_Gsl_Edge_Set (Obj) then
         return Gsl_Type (Create_Gsl_Edge_Id
           (Giant.Graph_Lib.Edge_Id_Sets.First
             (Get_Value (Gsl_Edge_Set (Obj)))));

      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_first': Gsl_Node_Set or Gsl_Edge_Set expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_First;

   ---------------------------------------------------------------------------
   --
   function Runtime_Size_Of
     (Parameter : Gsl_List)
      return Gsl_Type is

      Obj  : Gsl_Type;
      Size : Gsl_Natural;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_List (Obj) then
         Size := Create_Gsl_Natural (Get_List_Size (Gsl_List (Obj)));
      elsif Is_Gsl_Node_Set (Obj) then
         Size := Create_Gsl_Natural (Giant.Graph_Lib.Node_Id_Sets.Size
                                      (Get_Value (Gsl_Node_Set (Obj))));
      elsif Is_Gsl_Edge_Set (Obj) then
         Size := Create_Gsl_Natural (Giant.Graph_Lib.Edge_Id_Sets.Size
                                      (Get_Value (Gsl_Edge_Set (Obj))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'size_of': Gsl_List, Gsl_Node_Set or " &
           "Gsl_Edge_Set expected.");
      end if;
      return Gsl_Type (Size);
   end Runtime_Size_Of;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Entry
     (Parameter : Gsl_List)
      return Gsl_Type is

      List  : Gsl_Type;
      Index : Gsl_Type;
      Res   : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_entry': Expecting 2 parameters.");
      end if;
      List := Get_Value_At (Parameter, 1);
      Index := Get_Value_At (Parameter, 2);
      if Is_Gsl_List (List) and Is_Gsl_Natural (Index) then
         if Get_List_Size (Gsl_List (List)) <
            Get_Value (Gsl_Natural (Index)) then
            Res := Get_Value_At (Gsl_List (List),
                              Get_Value (Gsl_Natural (Index)));
         else
            Res := Gsl_Null;
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_entry': Gsl_List and Gsl_Natural expected.");
      end if;
      return Res;
   end Runtime_Get_Entry;

------------------------------------------------------------------------------
-- types (ref. GIANT Scripting Language Specification 1.5.1.6)

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Nodeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Node_Id (Param)));
   end Runtime_Is_Nodeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edgeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edgeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Edge_Id (Param)));
   end Runtime_Is_Edgeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_node_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Node_Set (Param)));
   end Runtime_Is_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edge_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Edge_Set (Param)));
   end Runtime_Is_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_String
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_string' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_String (Param)));
   end Runtime_Is_String;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Boolean
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_boolean' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Boolean (Param)));
   end Runtime_Is_Boolean;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Natural
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_natural' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Natural (Param)));
   end Runtime_Is_Natural;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_List
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_list' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_List (Param)));
   end Runtime_Is_List;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Reference
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_reference' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Var_Reference (Param)));
   end Runtime_Is_Reference;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Script
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_script' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      return Gsl_Type (Create_Gsl_Boolean (Is_Gsl_Script_Reference (Param)));
   end Runtime_Is_Script;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Null
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Null;

------------------------------------------------------------------------------
-- IML interpreter (ref. GIANT Scripting Language Specification 1.5.2.1)

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Current_Window
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Gsl.Interpreters;
   begin
      if Giant.Gsl.Interpreters.Get_Current_Context = "" then
         return Gsl_Null;
      else
         return Gsl_Type (Create_Gsl_String (Get_Current_Context));
      end if;
   end Runtime_Get_Current_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Set_Current_Window
     (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Controller;
      use Giant.Gsl.Interpreters;
      Name : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set_current_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         if Exists_Window (Get_Value (Gsl_String (Name))) then 
            Set_Current_Context(Get_Value (Gsl_String (Name)));
            return Gsl_Type (Create_Gsl_Boolean (true));
         else
            return Gsl_Type (Create_Gsl_Boolean (false));
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'set_current_window': Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Set_Current_Window;

------------------------------------------------------------------------------
-- IML graph (ref. GIANT Scripting Language Specification 1.5.1.7)

   ---------------------------------------------------------------------------
   --
   function Runtime_Root_Node
     (Parameter : Gsl_List)
      return Gsl_Type is

      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'root_node': Expecting no parameters.");
      end if;
      Node := Create_Gsl_Node_Id;
      Set_Value (Node, Graph_Lib.Get_Root_Node);
      return Gsl_Type (Node);
   end Runtime_Root_Node;

   ---------------------------------------------------------------------------
   --
   function Runtime_All_Nodes
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'all_nodes': Expecting no parameters.");
      end if;
      return Gsl_Type (Create_Gsl_Node_Set (Graph_Lib.Get_All_Nodes));
   end Runtime_All_Nodes;

   ---------------------------------------------------------------------------
   --
   function Runtime_Has_Attribute
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Graph_Lib;
      n      : Gsl_Type;
      attrib : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'has_attribute': Expecting 2 parameters.");
      end if;
      n := Get_Value_At (Parameter, 1);
      attrib := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Id (n) and Is_Gsl_String (attrib) then
         return Gsl_Type (Create_Gsl_Boolean (Does_Node_Attribute_Exist
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (n))),
            Get_Value (Gsl_String (attrib)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'has_attribute': Gsl_Node_Id and Gsl_String expected.");
      end if;
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Has_Attribute;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Attribute
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Graph_Lib;
      n         : Gsl_Type;
      attrib    : Gsl_Type;
      attrib_id : Node_Attribute_Id;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_attribute': Expecting 2 parameters.");
      end if;
      n := Get_Value_At (Parameter, 1);
      attrib := Get_Value_At (Parameter, 2);
      if Is_Gsl_Node_Id (n) and Is_Gsl_String (attrib) then
         if Does_Node_Attribute_Exist
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (n))),
            Get_Value (Gsl_String (attrib)))
         then
            attrib_id := Convert_Node_Attribute_Name_To_Id
              (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (n))),
               Get_Value (Gsl_String (attrib)));
            case Get_Node_Attribute_Class_Id (attrib_id) is
               when Class_Node_Id =>
                  return Gsl_Type (Create_Gsl_Node_Id
                    (Get_Node_Attribute_Node_Id_Value
                      (Get_Value (Gsl_Node_Id (n)), attrib_id)));

               when Class_Node_Id_List =>
                  null;

               when Class_Node_Id_Set =>
                  null;

               when Class_String =>
                  return Gsl_Type (Create_Gsl_String
                    (Get_Node_Attribute_String_Value
                      (Get_Value (Gsl_Node_Id (n)), attrib_id)));

               when Class_SLoc =>
                  null;

               when Class_Boolean =>
                  return Gsl_Type (Create_Gsl_Boolean
                    (Get_Node_Attribute_Boolean_Value
                      (Get_Value (Gsl_Node_Id (n)), attrib_id)));

               when Class_Natural =>
                  return Gsl_Type (Create_Gsl_Natural
                    (Get_Node_Attribute_Natural_Value
                      (Get_Value (Gsl_Node_Id (n)), attrib_id)));

            end case;
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_attribute': Gsl_Node_Id and Gsl_String expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Attribute; 

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Type
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Graph_Lib;
      Obj : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_type': Expecting 1 parameter.");
      end if;
      Obj := Get_Value_At (Parameter, 1);
      if Is_Gsl_Node_Id (Obj) then
         return Gsl_Type (Create_Gsl_String (Get_Node_Class_Tag
           (Get_Node_Class_Id (Get_Value (Gsl_Node_Id (Obj))))));

      elsif Is_Gsl_Edge_Id (Obj) then
         return Gsl_Type (Create_Gsl_String (Get_Edge_Class_Tag
           (Get_Edge_Class_Id (Get_Value (Gsl_Edge_Id (Obj))))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_type': Gsl_Node_Id or Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Type;


   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Source
      (Parameter : Gsl_List)
      return Gsl_Type is

      Edge : Gsl_Type;
      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_source': Expecting 1 parameter.");
      end if;
      Edge := Get_Value_At (Parameter, 1);
      if Is_Gsl_Edge_Id (Edge) then
         Node := Create_Gsl_Node_Id;
         Set_Value (Node, Graph_Lib.Get_Source_Node
           (Get_Value (Gsl_Edge_Id (Edge))));
         return Gsl_Type (Node);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_source': Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Source;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Target
      (Parameter : Gsl_List)
      return Gsl_Type is

      Edge : Gsl_Type;
      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_target': Expecting 1 parameter.");
      end if;
      Edge := Get_Value_At (Parameter, 1);
      if Is_Gsl_Edge_Id (Edge) then
         Node := Create_Gsl_Node_Id;
         Set_Value (Node, Graph_Lib.Get_Target_Node
           (Get_Value (Gsl_Edge_Id (Edge))));
         return Gsl_Type (Node);
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'get_target': Gsl_Edge_Id expected.");
      end if;
      return Gsl_Null;
   end Runtime_Get_Target;

------------------------------------------------------------------------------
-- GUI (ref. GIANT Scripting Language Specification 1.5.2.3 and 1.5.2.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Exists_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      Name : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'exists_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         return Gsl_Type (Create_Gsl_Boolean
           (Giant.Controller.Exists_Window (Get_Value (Gsl_String (Name)))));
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Gsl_String expected.");
      end if;   
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Exists_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Window_Content
      (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Get_Window_Content;

   ---------------------------------------------------------------------------
   --
   function Runtime_Create_Window
      (Parameter : Gsl_List)
      return Gsl_Type is

      use Giant.Controller;
      Name: Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Expecting 1 parameter.");
      end if;
      Name := Get_Value_At (Parameter, 1);
      if Is_Gsl_String (Name) then
         if Exists_Window (Get_Value (Gsl_String (Name)))
         then
            return Gsl_Type (Create_Gsl_Boolean (false));
         else
            Create_Window (Get_Value (Gsl_String (Name)));
            return Gsl_Type (Create_Gsl_Boolean (true));
         end if;
      else
         Ada.Exceptions.Raise_Exception (Gsl_Runtime_Error'Identity,
           "Script 'create_window': Gsl_String expected.");
      end if;
      return Gsl_Type (Create_Gsl_Boolean (false));
   end Runtime_Create_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Insert_Into_Window
      (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      --procedure Insert_Selection
      --  (Window_Name           : in String;
      --   Selection_Name        : in String;
      --   Selection             : in Graph_Lib.Selections.Selection;
      --   Layout_Name           : in String;
      --   Position              : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d;
      --   Additional_Parameters : in String );
      return Gsl_Null;
   end Runtime_Insert_Into_Window;

   ---------------------------------------------------------------------------
   --
   function Runtime_Remove_From_Window
      (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      --function Remove_Selection
      --  (Window_Name          : in String;
      --   Name                 : in String;
      --   Remove_Content       : in Boolean;
      --   Ask_For_Confirmation : in Boolean := True)
      --   return Boolean;
      return Gsl_Null;
   end Runtime_Remove_From_Window;

end Giant.Gsl.Runtime;
