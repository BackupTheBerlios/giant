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
-- $Date: 2003/06/02 11:30:06 $
--
-- This package implements the datatypes used in GSL.
--

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

   type Syntax_Node is private;
   Null_Node : constant Syntax_Node;

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

   type Syntax_Node is access Syntax_Node_Record;
   Null_Node : constant Syntax_Node := null;

end Giant.Gsl;
