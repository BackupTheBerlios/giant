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
-- $RCSfile: giant-syntax_tree.adb,v $, $Revision: 1.1 $
-- $Author: schulzgt $
-- $Date: 2003/05/27 13:44:17 $
--
with Unchecked_Deallocation;

package body Giant.Syntax_Tree is

   procedure Free is new Unchecked_Deallocation
     (Syntax_Node_Record,
      Syntax_Node);

   ---------------------------------------------------------------------------
   -- creates a new Syntax_Node
   function Create_Node
     (N_Type : Node_Type;
      Child1 : Syntax_Node;
      Child2 : Syntax_Node)
      return Syntax_Node is

      Node : Syntax_Node;
   begin
      Node := new Syntax_Node_Record;
      Node.N_Type := N_Type;
      Node.Child1 := Child1;
      Node.Child2 := Child2;
      return Node;
   end Create_Node;

   ---------------------------------------------------------------------------
   -- destroys a Syntax_Node with all children and frees the memory
   procedure Destroy_Node
     (Node : in out Syntax_Node) is
   begin
      if Node.Child1 /= Null_Node then
         Destroy_Node (Node.Child1);
      end if;
      if Node.Child2 /= Null_Node then
         Destroy_Node (Node.Child2);
      end if;
      Free (Node);
   end Destroy_Node;

   ---------------------------------------------------------------------------
   -- get the type of a Syntax_Node
   function Get_Node_Type
     (Node : Syntax_Node)
      return Node_Type is
   begin
      return Node.N_Type;
   end Get_Node_Type;
      
   ---------------------------------------------------------------------------
   -- get the 1st child of a Syntax_Node
   function Get_Child1
     (Node : Syntax_Node)
      return Syntax_Node is
   begin
      return Node.Child1;
   end Get_Child1;

   ---------------------------------------------------------------------------
   -- get the 2nd child of a Syntax_Node
   function Get_Child2
     (Node : Syntax_Node)
      return Syntax_Node is
   begin
      return Node.Child2;
   end Get_Child2;
	
end Giant.Syntax_Tree;
