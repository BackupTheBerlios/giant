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
-- $RCSfile: giant-gsl.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/06/10 11:56:20 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

-- from Bauhaus Reuse
with Stacks_Unbounded;
pragma Elaborate (Stacks_Unbounded);

with Hashed_Mappings;
pragma Elaborate (Hashed_Mappings);

with Giant.Default_Logger;

package Giant.Gsl is

   ---------------------------------------------------------------------------
   -- Gsl_Type - parent class for all types in GSL defined in Gsl.Types
   type Gsl_Type_Record is abstract tagged private;
   type Gsl_Type is access all Gsl_Type_Record'Class;

   ---------------------------------------------------------------------------
   -- represents the type Gsl_Null and works as null-pointer for Gsl_Type 
   Gsl_Null : constant Gsl_Type;

   type Node_Type is (Literal, Visible_Var, Global_Var, Visible_Ref,
                       Var_Creation, Global_Ref, Script_Decl, List, Sequence,
                       Script_Activation);

   type Syntax_Node_Record is private;
   type Syntax_Node is access all Syntax_Node_Record;

   Null_Node : constant Syntax_Node;

   procedure Log_Syntax_Node
     (Node : Syntax_Node);

   ---------------------------------------------------------------------------
   -- from Reuse
   package Execution_Stacks is new Stacks_Unbounded
     (Elem_Type => Syntax_Node);

   package Result_Stacks is new Stacks_Unbounded
     (Elem_Type => Gsl_Type);

   ---------------------------------------------------------------------------
   -- 
   type Gsl_Var_Record is private;
   type Gsl_Var is access all Gsl_Var_Record;

   ---------------------------------------------------------------------------
   -- instantiation of Hashed_Mappings for GSL variables
   -- the hash function Script_Hash uses String_Hash
   function Gsl_Var_Hash
     (K : Unbounded_String)
      return Integer;

   package Gsl_Var_Hashed_Mappings is new Hashed_Mappings
     (Key_Type => Unbounded_String,
      Value_Type => Gsl_Var,
      Hash => Gsl_Var_Hash);

   ---------------------------------------------------------------------------
   -- Activation Record 
   type Activation_Record_Record is private;
   type Activation_Record is access all Activation_Record_Record;

private 

   ---------------------------------------------------------------------------
   -- Gsl_Type - parent class for all types in GSL defined in Gsl.Types
   type Gsl_Type_Record is abstract tagged
      record
         null;
      end record;

   type Gsl_Type_Array is array (Positive range <>) of Gsl_Type;
   Gsl_Null : constant Gsl_Type := null;

   ---------------------------------------------------------------------------
   -- type for the nodes in the syntax tree
   type Syntax_Node_Record is
      record
         N_Type  : Node_Type;
         Child1  : Syntax_Node;
         Child2  : Syntax_Node;
         Literal : Gsl_Type;
         Size    : Natural;
     end record;

   Null_Node : constant Syntax_Node := null;

   ---------------------------------------------------------------------------
   --
   type Gsl_Var_Record is
      record
         Name  : Unbounded_String;
         Value : Gsl_Type;
      end record;

   ---------------------------------------------------------------------------
   --
   type Activation_Record_Record is
      record
         Parent : Activation_Record; 
         Vars   : Gsl_Var_Hashed_Mappings.Mapping;
      end record;

end Giant.Gsl;
