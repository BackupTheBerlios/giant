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
-- $RCSfile: giant-syntax_tree.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/05/27 13:44:17 $
--
-- This package implements the syntax tree used by GSL.
--
package Giant.Syntax_Tree is

   ---------------------------------------------------------------------------
   -- type for the nodes in the syntax tree
   type Syntax_Node is private;

   Null_Node : constant Syntax_Node;

   ---------------------------------------------------------------------------
   -- all possible types of a Syntax_Node
   type Node_Type is (Literal, Visible_Var, Global_Var, Visible_Ref,
                      Var_Creation, Global_Ref, Script_Decl, List, Sequence,
		      Script_Activation);
		      
   ---------------------------------------------------------------------------
   -- creates a new Syntax_Node
   function Create_Node
     (N_Type : Node_Type;
      Child1 : Syntax_Node;
      Child2 : Syntax_Node)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node with all children and frees the memory
   procedure Destroy_Node
     (Node : in out Syntax_Node);

   ---------------------------------------------------------------------------
   -- get the type of a Syntax_Node
   function Get_Node_Type
     (Node : Syntax_Node)
      return Node_Type;
      
   ---------------------------------------------------------------------------
   -- get the 1st child of a Syntax_Node
   function Get_Child1
     (Node : Syntax_Node)
      return Syntax_Node;

   ---------------------------------------------------------------------------
   -- get the 2nd child of a Syntax_Node
   function Get_Child2
     (Node : Syntax_Node)
      return Syntax_Node;
     
------------------------------------------------------------------------------
-- private part
private
   type Syntax_Node_Record is tagged
      record
         N_Type  : Node_Type;
	 Child1  : Syntax_Node;
	 Child2  : Syntax_Node;
	 --Value   : Gsl_Type;
	 Size    : Natural;
      end record;

   type Syntax_Node is access Syntax_Node_Record;      

   Null_Node : constant Syntax_Node := null;
   
end Giant.Syntax_Tree;
