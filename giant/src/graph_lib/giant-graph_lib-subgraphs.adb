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
--  $RCSfile: giant-graph_lib-subgraphs.adb,v $, $Revision: 1.9 $
--  $Author: koppor $
--  $Date: 2003/06/28 22:17:30 $

package body Giant.Graph_Lib.Subgraphs is

   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Subgraph;
      Right : in Subgraph)
      return Boolean
   is
   begin
      return Selections."<" ( Selections.Selection (Left),
                              Selections.Selection (Right));
   end "<";

   ---------------------------------------------------------------------------
   procedure Add_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge               : in     Edge_Id)
   is
   begin
      if Selections.Is_Member
        (Selections.Selection (Subgraph_To_Modify),
         Edge.Source_Node)
        and
        Selections.Is_Member
        (Selections.Selection (Subgraph_To_Modify),
         Edge.Target_Node)
      then
         Selections.Add_Edge
           (Selections.Selection (Subgraph_To_Modify),
            Edge);
      end if;
   end Add_Edge;

   ---------------------------------------------------------------------------
   procedure Add_Edge_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set           : in     Edge_Id_Set)
   is

      procedure Execute (Edge : in Edge_Id) is
      begin
         Add_Edge (Subgraph_To_Modify,
                   Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Edge_Set);
   end Add_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Add_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node               : in     Node_Id)
   is
   begin
      Selections.Add_Node
        (Selections.Selection (Subgraph_To_Modify),
         Node);
   end Add_Node;

   ---------------------------------------------------------------------------
   procedure Add_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set           : in     Node_Id_Set)
   is
   begin
      Selections.Add_Node_Set
        (Selections.Selection (Subgraph_To_Modify),
         Node_Set);
   end Add_Node_Set;

   ---------------------------------------------------------------------------
   function Clone
     (SubGraph_To_Clone : in Subgraph;
      Name_Of_Result    : in String)
      return Subgraph
   is
   begin
      return Subgraph (Selections.Clone
                       (Selections.Selection (SubGraph_To_Clone),
                        Name_Of_Result));
   end Clone;

   ---------------------------------------------------------------------------
   function Create
     (Name : in    String)
      return Subgraph
   is
   begin
      return Subgraph (Selections.Create (Name));
   end Create;

   ---------------------------------------------------------------------------
   function Create
     (Name                 : in String;
      Selection_To_Convert : in Graph_Lib.Selections.Selection)
      return Subgraph
   is
      Res : Subgraph;
   begin
      Res := Subgraph (Selections.Clone (Selection_To_Convert, Name));
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Create;

   ---------------------------------------------------------------------------
   --  Returns a clone, since the caller may not change our internal data
   function Create_Selection
     (Source   : in Subgraph;
      New_Name : in String)
      return Graph_Lib.Selections.Selection
   is
   begin
      return Selections.Clone (Selections.Selection (Source), New_Name);
   end Create_Selection;

   ---------------------------------------------------------------------------
   --  It is intended not to return a clone
   function Convert_To_Selection
     (Source : in Subgraph)
     return Graph_Lib.Selections.Selection
   is
   begin
      return Selections.Selection (Source);
   end Convert_To_Selection;

   ---------------------------------------------------------------------------
   procedure Destroy
     (SubGraph_To_Destroy : in out Subgraph)
   is
   begin
      Selections.Destroy
        (Selections.Selection (SubGraph_To_Destroy));
   end Destroy;

   ---------------------------------------------------------------------------
   function Get_Edge_Count
     (Graph : in Subgraph)
     return Natural
   is
   begin
      return Selections.Get_Edge_Count
        (Selections.Selection (Graph));
   end Get_Edge_Count;

   ---------------------------------------------------------------------------
   function Get_Name
     (Subgraph_To_Read : in Subgraph)
      return String
   is
   begin
      return Selections.Get_Name
        (Selections.Selection (Subgraph_To_Read));
   end Get_Name;

   ---------------------------------------------------------------------------
   function Get_Node_Count
     (Graph : in Subgraph)
     return Natural
   is
   begin
      return Selections.Get_Node_Count
        (Selections.Selection (Graph));
   end Get_Node_Count;

   ---------------------------------------------------------------------------
   function Intersection
     (Left           : in Subgraph;
      Right          : in Subgraph;
      Name_Of_Result : in String)
      return Subgraph
   is
      Res : Subgraph;
   begin
      Res := Subgraph (Selections.Intersection
                       (Selections.Selection (Left),
                        Selections.Selection (Right),
                        Name_Of_Result));
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Intersection;

   ---------------------------------------------------------------------------
   procedure Remove_Edge
     (Subgraph_To_Modify : in out Subgraph;
      Edge               : in     Edge_Id)
   is
   begin
      Selections.Remove_Edge
        (Selections.Selection (Subgraph_To_Modify),
         Edge);
   end Remove_Edge;

   ---------------------------------------------------------------------------
   procedure Remove_Edge_Set
     (Subgraph_To_Modify : in out Subgraph;
      Edge_Set           : in     Edge_Id_Set)
   is
   begin
      Selections.Remove_Edge_Set
        (Selections.Selection (Subgraph_To_Modify),
         Edge_Set);
   end Remove_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Remove_Node
     (Subgraph_To_Modify : in out Subgraph;
      Node               : in     Node_Id)
   is

      ----------------------------------------------------------------------
      --  Removes edges in the subgraph, if they were connected with given
      --    node
      --
      --  Destroys given set after checking
      --
      --  Parameters:
      --    Edges_In : Edges to check,
      --               all incomig or outgoing edges of a node
      procedure Check_Edges (Edges_In : in Edge_Id_Set)
      is
         Edges : Edge_Id_Set  := Edges_In;

         procedure Execute (Edge : in Edge_Id)
         is
         begin
            begin
               Selections.Remove_Edge
                 (Selections.Selection (Subgraph_To_Modify), Edge);
            exception
               when Edge_Does_Not_Exist =>
                  --  simulates a "Remove_If_Exists"
                  null;
            end;
         end Execute;

         procedure Apply is new Edge_Id_Sets.Apply
           (Execute => Execute);

      begin
         Apply (Edges);
         Edge_Id_Sets.Destroy (Edges);
      end Check_Edges;

      Edges : Edge_Id_Set;

   begin
      Selections.Remove_Node
        (Selections.Selection (Subgraph_To_Modify),
         Node);

      --  Check_Edges also destroys the given set
      --    therefore there is no memory leak produced here
      --  Optimization:
      --    Write "apply" for Edges of a node
      --      --> this routine could traverse directly the array of edges
      --          at the node and there is no need anymore to convert that
      --          array to an Edge_Id_Set
      Check_Edges (Get_Incoming_Edges (Node));
      Check_Edges (Get_Outgoing_Edges (Node));
   end Remove_Node;

   ---------------------------------------------------------------------------
   procedure Remove_Node_Set
     (Subgraph_To_Modify : in out Subgraph;
      Node_Set           : in     Node_Id_Set)
   is

      procedure Execute (Node : in Node_Id) is
      begin
         Remove_Node (Subgraph_To_Modify,
                      Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      if Node_Id_Sets.Size (Node_Set) <=
        (Selections.Get_Node_Count
         (Selections.Selection (Subgraph_To_Modify)) / 2) then
         --  if less than half of the nodes are to be removed,
         --    it's faster to check all incoming and outgoing edges
         --    of the nodes to be removed
         Apply (Selections.Get_All_Nodes
                (Selections.Selection (Subgraph_To_Modify)));
      else
         --  if more than half of the nodes are to be removed
         --    it's faster to check all edges in the selection
         --    if they are still valid
         --
         --  TBD: CHECK: maybe .25 is even more faster
         Selections.Remove_Node_Set
           (Selections.Selection (Subgraph_To_Modify),
            Node_Set);
         Ensure_Graph_Edge_Properties (Subgraph_To_Modify);
      end if;
   end Remove_Node_Set;

   ---------------------------------------------------------------------------
   procedure Rename
     (SubGraph_To_Rename : in out Subgraph;
      New_Name           : in     String)
   is
   begin
      Selections.Rename
        (Selections.Selection (Subgraph_To_Rename),
         New_Name);
   end Rename;

   ---------------------------------------------------------------------------
   procedure Subgraph_Read
     (Stream            : in     Bauhaus_Io.In_Stream_Type;
      Subgraph_To_Read  :    out Subgraph)
   is
   begin
      Selections.Selection_Read
        (Stream,
         Selections.Selection (Subgraph_To_Read));
   end Subgraph_Read;

   ---------------------------------------------------------------------------
   procedure Subgraph_Write
     (Stream            : in Bauhaus_Io.Out_Stream_Type;
      Subgraph_To_Write : in Subgraph)
   is
   begin
      Selections.Selection_Write
        (Stream,
         Selections.Selection (Subgraph_To_Write));
   end Subgraph_Write;

   ---------------------------------------------------------------------------
   function Symetric_Difference
     (Left           : in Subgraph;
      Right          : in Subgraph;
      Name_Of_Result : in String)
      return Subgraph
   is
      Res : Subgraph;
   begin
      Res := Subgraph (Selections.Symetric_Difference
                       (Selections.Selection (Left),
                        Selections.Selection (Right),
                        Name_Of_Result));
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Symetric_Difference;

   ---------------------------------------------------------------------------
   function Union
     (Left           : in Subgraph;
      Right          : in Subgraph;
      Name_Of_Result : in String)
      return Subgraph
   is
      Res : Subgraph;
   begin
      Res := Subgraph (Selections.Union
                       (Selections.Selection (Left),
                        Selections.Selection (Right),
                        Name_Of_Result));
      Ensure_Graph_Edge_Properties (Res);
      return Res;
   end Union;

   ---------------------------------------------------------------------------
   procedure Ensure_Graph_Edge_Properties
     (Graph : in out Subgraph;
      Edge  : in     Edge_Id)
   is
      Source_And_Target_In_Subgraph : Boolean;
   begin
      Source_And_Target_In_Subgraph :=
        Selections.Is_Member
        (Selections.Selection (Graph),
         Edge.Source_Node) and
        Selections.Is_Member
        (Selections.Selection (Graph),
         Edge.Target_Node);

      if not Source_And_Target_In_Subgraph then
         --  "Edge_Does_Not_Exist" could be risen here.
         --     It is not catched nor converted, since the preconditions
         --     assures, that this cannot happen
         Selections.Remove_Edge (Selections.Selection (Graph), Edge);
      end if;
   end Ensure_Graph_Edge_Properties;

   ---------------------------------------------------------------------------
   procedure Ensure_Graph_Edge_Properties (Graph : in out Subgraph)
   is

      procedure Execute (Edge : in Edge_Id)
      is
      begin
         Ensure_Graph_Edge_Properties (Graph, Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply
        (Execute => Execute);

      --  since edges are removed during the traversal through the edge_set
      --    and the iterator cannot deal with sets being modified during the
      --    traversal, a clone has to be used
      Clone : Edge_Id_Sets.Set;

   begin
      Clone := Edge_Id_Sets.Copy
        (Selections.Get_All_Edges (Selections.Selection (Graph)));
      Apply (Clone);
      Edge_Id_Sets.Destroy (Clone);
   end Ensure_Graph_Edge_Properties;

end Giant.Graph_Lib.Subgraphs;
