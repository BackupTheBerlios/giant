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
--  $RCSfile: giant-graph_lib-subgraphs.adb,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/06/17 13:08:26 $

package body Giant.Graph_Lib.Subgraphs is

   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Boolean
   is
   begin
      return Selections."<" (Left.Sel, Right.Sel);
   end "<";

   ---------------------------------------------------------------------------
   procedure Add_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge                : in     Edge_Id)
   is
   begin
      Selections.Add_Edge
        (Subgraph_To_Modify.Sel,
         Edge);

      -- TBD: This could go faster, if we'd only check the added edge
      Ensure_Graph_Edge_Properties (Subgraph_To_Modify);
   end Add_Edge;

   ---------------------------------------------------------------------------
   procedure Add_Edge_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set           : in     Edge_Id_Set)
   is
   begin
      Selections.Add_Edge_Set
        (Subgraph_To_Modify.Sel,
         Edge_Set);

      -- TBD: This could go faster, if we'd only check the added edges
      Ensure_Graph_Edge_Properties (Subgraph_To_Modify);
   end Add_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Add_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node               : in     Node_Id)
   is
   begin
      Selections.Add_Node
        (Subgraph_To_Modify.Sel,
         Node);
   end Add_Node;

   ---------------------------------------------------------------------------
   procedure Add_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set           : in     Node_Id_Set)
   is
   begin
      Selections.Add_Node_Set
        (Subgraph_To_Modify.Sel,
         Node_Set);
   end Add_Node_Set;

   ---------------------------------------------------------------------------
   function Clone
     (SubGraph_To_Clone : in Subgraph)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Clone (SubGraph_To_Clone.Sel);
      return Res;
   end Clone;

   ---------------------------------------------------------------------------
   function Create
     (Name : in    String)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Create (Name);
      return Res;
   end Create;

   ---------------------------------------------------------------------------
   function Create
     (Name                 : in String;
      Selection_To_Convert : in Graph_Lib.Selections.Selection)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Clone (Selection_To_Convert);
      Rename (Res, Name);
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy
     (SubGraph_To_Destroy : in out Subgraph)
   is
   begin
      Selections.Destroy (SubGraph_To_Destroy.Sel);
      --  TBD: unchecked_dealloc
   end Destroy;

   ---------------------------------------------------------------------------
   function Get_Name
     (Subgraph_To_Read : in Subgraph)
      return String
   is
   begin
      return Selections.Get_Name (Subgraph_To_Read.Sel);
   end Get_Name;

   ---------------------------------------------------------------------------
   --  Returns a clone, since the caller may not change our internal data
   function Get_Selection
     (Subgraph_To_Read : in Subgraph)
      return Graph_Lib.Selections.Selection
   is
   begin
      return Selections.Clone (Subgraph_To_Read.Sel);
   end Get_Selection;

   ---------------------------------------------------------------------------
   function Intersection
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Intersection (Left.Sel, Right.Sel);
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Intersection;

   ---------------------------------------------------------------------------
   procedure Remove_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge               : in     Edge_Id)
   is
   begin
      Selections.Remove_Edge (Subgraph_To_Modify.Sel, Edge);
   end Remove_Edge;

   ---------------------------------------------------------------------------
   procedure Remove_Edge_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set           : in     Edge_Id_Set)
   is
   begin
      Selections.Remove_Edge_Set (Subgraph_To_Modify.Sel, Edge_Set);
   end Remove_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Remove_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node                : in     Node_Id)
   is
   begin
      Selections.Remove_Node (Subgraph_To_Modify.Sel, Node);
   end Remove_Node;

   ---------------------------------------------------------------------------
   procedure Remove_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set           : in     Node_Id_Set)
   is
   begin
      Selections.Remove_Node_Set (Subgraph_To_Modify.Sel, Node_Set);
   end Remove_Node_Set;

   ---------------------------------------------------------------------------
   procedure Rename
     (SubGraph_To_Rename : in out Subgraph;
      New_Name           : in     String)
   is
   begin
      Selections.Rename (Subgraph_To_Rename.Sel, New_Name);
   end Rename;

   ---------------------------------------------------------------------------
   procedure Subgraph_Read
     (Stream            : in     Bauhaus_Io.In_Stream_Type;
      Subgraph_To_Read  :    out Subgraph)
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Selections.Selection_Read (Stream, Subgraph_To_Read.Sel);
   end Subgraph_Read;

   ---------------------------------------------------------------------------
   procedure Subgraph_Write
     (Stream            : in Bauhaus_Io.Out_Stream_Type;
      Subgraph_To_Write : in Subgraph)
   is
   begin
      Selections.Selection_Write (Stream, Subgraph_To_Write.Sel);
   end Subgraph_Write;

   ---------------------------------------------------------------------------
   function Symetric_Difference
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Symetric_Difference (Left.Sel, Right.Sel);
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Symetric_Difference;

   ---------------------------------------------------------------------------
   function Union
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Subgraph
   is
      Res : Subgraph := new Subgraph_Record;
   begin
      Res.Sel := Selections.Union (Left.Sel, Right.Sel);
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Union;

   ---------------------------------------------------------------------------
   procedure Ensure_Graph_Edge_Properties (Graph : in out Subgraph)
   is
   begin
      --  TBD
      null;
   end Ensure_Graph_Edge_Properties;

end Giant.Graph_Lib.Subgraphs;
