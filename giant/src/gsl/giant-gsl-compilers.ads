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
-- $RCSfile: giant-gsl-compilers.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/06/06 20:03:34 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Hashed_Mappings;
pragma Elaborate (Hashed_Mappings);

package Giant.Gsl.Compilers is

   type Compiler_Record is private;
   type Compiler is access all Compiler_Record;

   ---------------------------------------------------------------------------
   --
   type Gsl_Script_Record (Name_Length : Natural) is private;
   type Gsl_Script is access all Gsl_Script_Record;

   ---------------------------------------------------------------------------
   -- instantiation of Hashed_Mappings for GSL scripts
   -- the hash function Script_Hash uses String_Hash
   function Script_Hash
     (K : Unbounded_String) 
      return Integer;

   package Script_Hashed_Mappings is new Hashed_Mappings
     (Key_Type => Unbounded_String,
      Value_Type => Gsl_Script,
      Hash => Script_Hash);

   ---------------------------------------------------------------------------
   --
   function Create_Compiler return Compiler;

   ---------------------------------------------------------------------------
   --
   procedure Destroy_Compiler
     (Comp : Compiler);

   ---------------------------------------------------------------------------
   --
   function Get_Execution_Stack
     (Comp : Compiler;
      Name : String)
      return Execution_Stacks.Stack;

   ---------------------------------------------------------------------------
   --
   function Get_Execution_Stack
     (Comp : Compiler;
      Node : Syntax_Node)
      return Execution_Stacks.Stack;

private

   ---------------------------------------------------------------------------
   --
   type Compiler_Record is
      record
         Scripts : Script_Hashed_Mappings.Mapping;
      end record ;

   ---------------------------------------------------------------------------
   --
   type Gsl_Script_Record (Name_Length : Natural) is
      record
         Name : String (1 .. Name_Length);
         Syntax_Tree : Syntax_Node;
      end record;

end Giant.Gsl.Compilers;
