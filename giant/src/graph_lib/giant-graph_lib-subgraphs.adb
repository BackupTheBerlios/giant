------------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  $RCSfile: giant-graph_lib-subgraphs.adb,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/06/10 09:20:49 $

package body Giant.Graph_Lib.Subgraphs is

   ---------
   -- "<" --
   ---------

   function "<"
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Boolean
   is
   begin
      return "<" (Left, Right);
   end "<";

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge                : in     Edge_Id)
   is
   begin
      null;
   end Add_Edge;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node                : in     Node_Id)
   is
   begin
      null;
   end Add_Node;

   ------------------
   -- Add_Node_Set --
   ------------------

   procedure Add_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set            : in     Node_Id_Set)
   is
   begin
      null;
   end Add_Node_Set;

   ------------------
   -- Add_Node_Set --
   ------------------

   procedure Add_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set            : in     Edge_Id_Set)
   is
   begin
      null;
   end Add_Node_Set;

   -----------
   -- Clone --
   -----------

   function Clone
     (SubGraph_To_Clone : in Subgraph)
      return Subgraph
   is
   begin
      return Clone (SubGraph_To_Clone);
   end Clone;

   ------------
   -- Create --
   ------------

   function Create
     (Name : in    Valid_Names.Standard_Name)
      return Subgraph
   is
   begin
      return Create (Name);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name : in Valid_Names.Standard_Name;
      Selection_To_Convert : in Graph_Lib.Selections.Selection)
      return Subgraph
   is
   begin
      return Create (Name, Selection_To_Convert);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (SubGraph_To_Destroy : in out Subgraph)
   is
   begin
      null;
   end Destroy;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Subgraph_To_Read : in Subgraph)
      return String
   is
   begin
      return Get_Name (Subgraph_To_Read);
   end Get_Name;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
      return Graph_Lib.Selections.Selection
   is
   begin
      return Get_Selection;
   end Get_Selection;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
   begin
      return Intersection (Left, Right);
   end Intersection;

   -----------------
   -- Remove_Edge --
   -----------------

   procedure Remove_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge                : in     Edge_Id)
   is
   begin
      null;
   end Remove_Edge;

   ---------------------
   -- Remove_Edge_Set --
   ---------------------

   procedure Remove_Edge_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set            : in     Edge_Id_Set)
   is
   begin
      null;
   end Remove_Edge_Set;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node                : in     Node_Id)
   is
   begin
      null;
   end Remove_Node;

   ---------------------
   -- Remove_Node_Set --
   ---------------------

   procedure Remove_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set            : in     Node_Id_Set)
   is
   begin
      null;
   end Remove_Node_Set;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (SubGraph_To_Rename : in out Subgraph;
      New_Name           : in     Valid_Names.Standard_Name)
   is
   begin
      null;
   end Rename;

   -------------------
   -- Subgraph_Read --
   -------------------

   procedure Subgraph_Read
     (Stream            : in Bauhaus_Io.In_Stream_Type;
      Subgraph_To_Read  : in Subgraph)
   is
   begin
      null;
   end Subgraph_Read;

   --------------------
   -- Subgraph_Write --
   --------------------

   procedure Subgraph_Write
     (Stream            : in Bauhaus_Io.Out_Stream_Type;
      Subgraph_To_Write : in Subgraph)
   is
   begin
      null;
   end Subgraph_Write;

   -------------------------
   -- Symetric_Difference --
   -------------------------

   function Symetric_Difference
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
   begin
      return Symetric_Difference (Left, Right);
   end Symetric_Difference;

   -----------
   -- Union --
   -----------

   function Union
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
   begin
      return Union (Left, Right);
   end Union;

end Giant.Graph_Lib.Subgraphs;

