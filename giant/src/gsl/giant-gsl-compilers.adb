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
-- $RCSfile: giant-gsl-compilers.adb,v $
-- $Author: schulzgt $
-- $Date: 2003/06/09 14:14:22 $
--
-- This package implements the datatypes used in GSL.
--

with Text_IO;

with String_Hash;

with Giant.Default_Logger;

with Giant.Parser;
with Giant.Scanner.IO;

with Giant.Gsl.Syntax_Tree;
use  Giant.Gsl.Syntax_Tree;

package body Giant.Gsl.Compilers is

   ---------------------------------------------------------------------------
   --
   function Script_Hash
     (K : Unbounded_String)
      return Integer is
   begin
      return String_Hash (To_String (K));
   end Script_Hash;

   ---------------------------------------------------------------------------
   -- Creates a new compiler for GSL scripts
   function Create_Compiler return Compiler is

      Comp : Compiler;
   begin
      Comp := new Compiler_Record;
      Comp.Scripts := Script_Hashed_Mappings.Create;
      return Comp;
   end Create_Compiler;

   ---------------------------------------------------------------------------
   -- destroys a compiler
   procedure Destroy_Compiler
     (Comp : Compiler) is
   begin
      null;
   end Destroy_Compiler;

   ---------------------------------------------------------------------------
   -- parser call
   function Get_Execution_Stack
     (Comp : Compiler;
      Name : String)
      return Execution_Stacks.Stack is

      Script : Gsl_Script; 
      Stack  : Execution_Stacks.Stack;
   begin
      Text_IO.Put ("Looking for " & Name & " in Hash Map: ");
      if Script_Hashed_Mappings.Is_Bound (Comp.Scripts, 
        To_Unbounded_String(Name)) then
         Text_IO.Put_Line ("Found");
         -- script was already parsed
         -- syntax tree is in the cache
         Script := Script_Hashed_Mappings.Fetch (Comp.Scripts,
           To_Unbounded_String(Name)); 
         return Get_Execution_Stack (Comp, Script.Syntax_Tree);
      else
         Text_IO.Put_Line ("Not Found");
         Script := new Gsl_Script_Record (Name'Length);
         Script.Name := Name;
 
        -- insert script in hash map
         Script_Hashed_Mappings.Bind (Comp.Scripts,
           To_Unbounded_String(Name), Script);

         -- use the gsl parser to generate a syntax tree
         Text_IO.Put_Line ("Start Parser..");
         Giant.Scanner.IO.Open_Input (Name & ".gsl");
         Giant.Parser.yyparse;
         Script.Syntax_Tree := Giant.Parser.Get_Syntax_Tree;
         Giant.Scanner.IO.Close_Input;
         return Get_Execution_Stack (Comp, Script.Syntax_Tree);
      end if;
   end Get_Execution_Stack;

   ---------------------------------------------------------------------------
   -- creates and builds a new Execution_Stack by a recursive traversing
   -- of the Syntax_Tree
   function Get_Execution_Stack
     (Comp : Compiler;
      Node : Syntax_Node)
      return Execution_Stacks.Stack is

      Stack : Execution_Stacks.Stack;
   begin
      Stack := Execution_Stacks.Create;
      Push_Syntax_Node (Node, Stack);
      return Stack;
   end Get_Execution_Stack;


   procedure Push_Syntax_Node
     (Node  :        Syntax_Node;
      Stack : in out Execution_Stacks.Stack) is
 
      Size  : Natural := 1;
   begin
      if Node /= Null_Node then
         if Get_Node_Type (Node) = Sequence or 
            Get_Node_Type (Node) = List then
            -- sequence or list needs a recursive traversation
            Default_Logger.Debug
              ("Compiler: List or Sequence found.", "Giant.Gsl");
            Execution_Stacks.Push (Stack, Node);
            if Get_Child1 (Node) = Null_Node then
               -- empty sequence or list, set size to 0
               Set_Size (Node, 0);
            else
               -- push all elements of the sequence
               Push_Sequence (Node, Stack, Size);
               -- set the size of the sequence
               Set_Size (Node, Size);
            end if;
            Log_Syntax_Node (Node);
         else
            Default_Logger.Debug ("Compiler: Push", "Giant.Gsl");
            Log_Syntax_Node (Node);
            Execution_Stacks.Push (Stack, Node);
         end if;
      end if;
   end Push_Syntax_Node;

   procedure Push_Sequence
     (Node  :        Syntax_Node;
      Stack : in out Execution_Stacks.Stack;
      Size  : in out Natural) is 

   begin
      if Get_Child2 (Node) = Null_Node then
         -- end of a sequence
         Push_Syntax_Node (Get_Child1 (Node), Stack);
      else
         -- recursive
         Size := Size + 1;
         Push_Sequence (Get_Child2 (Node) , Stack, Size);
         Push_Syntax_Node (Get_Child1 (Node), Stack);
      end if;
   end Push_Sequence;

end Giant.Gsl.Compilers;
