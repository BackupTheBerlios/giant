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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-graph_lib-subgraphs.ads,v $, $Revision: 1.12 $
--  $Author: koppor $
--  $Date: 2003/06/28 22:18:08 $
--
------------------------------------------------------------------------------
--
--  Contains the subgraphs
--
--  Will extend routines of Graph_Lib.Selections to ensure, that result
--    is a subgraph
--
--  no inheriance was used, since this is too oversized

--  from Bauhaus
with Lists;
with Bauhaus_Io;

--  from GIANT
with Giant.Graph_Lib.Selections;

package Giant.Graph_Lib.Subgraphs is

   ---------------------------------------------------------------------------
   --  It is assured, that a copy makes an alias
   type Subgraph is private;

   ---------------------------------------------------------------------------
   --  Creates a new subgraph,
   --    which has to be destroyed afterwards
   function Create
      (Name : in    String)
      return Subgraph;

   ---------------------------------------------------------------------------
   --  Creates a new subgraph,
   --    which has to be destroyed afterwards
   --
   --  Parameters:
   --    Selection : Selection to convert to a subgraph
   --  Returns:
   --    Selection converted to a subgraph
   function Create
     (Name                 : in String;
      Selection_To_Convert : in Graph_Lib.Selections.Selection)
     return Subgraph;

   ---------------------------------------------------------------------------
   --  Removes Subgraph given from memory
   procedure Destroy
      (SubGraph_To_Destroy : in out Subgraph);

   ---------------------------------------------------------------------------
   --  Creates a deep-copy of the selection
   function Clone
     (SubGraph_To_Clone : in Subgraph;
      Name_Of_Result    : in String)
     return Subgraph;

   ---------------------------------------------------------------------------
   procedure Rename
      (SubGraph_To_Rename : in out Subgraph;
       New_Name           : in     String);

   ---------------------------------------------------------------------------
   function Get_Name
     (Subgraph_To_Read : in Subgraph)
     return String;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : in Subgraph;
       Right : in Subgraph)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of edges contained in given subgraph
   function Get_Edge_Count
     (Graph : in Subgraph)
     return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Number of nodes contained in given subgraph
   function Get_Node_Count
     (Graph : in Subgraph)
     return Natural;

   ---------------
   --  Streams  --
   ---------------

   ---------------------------------------------------------------------------
   --  Notes:
   --    cp. Barnes p543
   --    Maybe u have to use "for Subgraph'Write use Subgraph_Write"
   procedure Subgraph_Write
      (Stream            : in Bauhaus_Io.Out_Stream_Type;
       Subgraph_To_Write : in Subgraph);

   ---------------------------------------------------------------------------
   procedure Subgraph_Read
      (Stream            : in     Bauhaus_Io.In_Stream_Type;
       Subgraph_To_Read  :    out Subgraph);

   -----------------------
   --  Routines to add  --
   -----------------------

   ---------------------------------------------------------------------------
   --  Adds given Node to the given Subgraph
   procedure Add_Node
      (Subgraph_To_Modify : in out Subgraph;
       Node                : in     Node_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Subgraph
   procedure Add_Node_Set
      (Subgraph_To_Modify : in out Subgraph;
       Node_Set            : in     Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Adds given Edge to the given Subgraph
   procedure Add_Edge
      (Subgraph_To_Modify : in out Subgraph;
       Edge                : in     Edge_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Subgraph
   procedure Add_Edge_Set
      (Subgraph_To_Modify : in out Subgraph;
       Edge_Set            : in     Edge_Id_Set);

   --------------------------
   --  Routines to remove  --
   --------------------------

   ---------------------------------------------------------------------------
   --  Adds given Node to the given Subgraph
   procedure Remove_Node
      (Subgraph_To_Modify : in out Subgraph;
       Node                : in     Node_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Subgraph
   procedure Remove_Node_Set
      (Subgraph_To_Modify : in out Subgraph;
       Node_Set            : in     Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Adds given Edge to the given Subgraph
   procedure Remove_Edge
      (Subgraph_To_Modify : in out Subgraph;
       Edge                : in     Edge_Id);

   ---------------------------------------------------------------------------
   --  Adds all edges in given set to the Subgraph
   procedure Remove_Edge_Set
      (Subgraph_To_Modify : in out Subgraph;
       Edge_Set            : in     Edge_Id_Set);

   ----------------------
   --  Set-operations  --
   ----------------------

   ---------------------------------------------------------------------------
   --  Creates a new selection where the two given Subgraphs are unified
   function Union
      (Left           : in Subgraph;
       Right          : in Subgraph;
       Name_Of_Result : in String)
      return Subgraph;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Symetric_Difference
      (Left           : in Subgraph;
       Right          : in Subgraph;
       Name_Of_Result : in String)
      return Subgraph;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Intersection
      (Left           : in Subgraph;
       Right          : in Subgraph;
       Name_Of_Result : in String)
      return Subgraph;

   -------------------
   --  Convertions  --
   -------------------

   ---------------------------------------------------------------------------
   --  Converts current subgraph to a selection
   --
   --  Source is not affected by modifying the result
   function Create_Selection
     (Source   : in Subgraph;
      New_Name : in String)
     return Graph_Lib.Selections.Selection;

   ---------------------------------------------------------------------------
   --  Converts current subgraph to a selection
   --    having the same name
   --
   --  The given subgraph is destroyed - you may not continue to use it
   function Convert_To_Selection
     (Source : in Subgraph)
     return  Graph_Lib.Selections.Selection;

private
   type Subgraph is new Selections.Selection;

   ---------------------------------------------------------------------------
   --  removes given edge if it has no source or no target in the given
   --  graph
   --
   --  Pre:
   --    given edge has to exist in given graph
   --  Raises:
   --    Edge_Does_Not_Exist if pre-condition is not satisfied
   procedure Ensure_Graph_Edge_Properties
     (Graph : in out Subgraph;
      Edge  : in     Edge_Id);

   ---------------------------------------------------------------------------
   --  removes all edges which have no source or no target
   procedure Ensure_Graph_Edge_Properties
     (Graph : in out Subgraph);

end Giant.Graph_Lib.Subgraphs;
