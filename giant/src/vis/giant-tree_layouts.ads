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
--  $RCSfile: giant-tree_layouts.ads,v $, $Revision: 1.11 $
--  $Author: koppor $
--  $Date: 2003/07/10 23:34:06 $
--
------------------------------------------------------------------------------
--
--  Contains the treelayout-algorithm
--

with Lists;
pragma Elaborate_All (Lists);

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

--  Speed optimization:
--    Include Last_Element-"hack" into Stacks_Unbounded
--    Otherwise Push has O(n)
with Stacks_Unbounded;
pragma Elaborate_All (Stacks_Unbounded);

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Evolutions;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Tree_Layouts is

   ---------------------------------------------------------------------------
   Root_Node_Not_In_Selection : exception;

   ---------------------------------------------------------------------------
   --  Concurrent can't be used, because of the stacks
   --    But the bounded-stackpackage could be used, since
   --    after Init_Run, the maximum height of the tree is known
   --    this is /not/ the max. size of the stack
   --    max size is tree-height * (degree-1) or size (Nodes_To_Layout)
   type Tree_Layout_Record is
     new Giant.Evolutions.Iterative_Evolution with private;
   type Tree_Layout is access Tree_Layout_Record;

   ---------------------------------------------------------------------------
   --  Maximum number of nost to be processed in one run
   Max_Nodes_In_One_Run : constant := 100;

   ---------------------------------------------------------------------------
   --  Distance of two neighbours as proportion of Get_Current_Maximum_Node_Width
   X_Distance           : constant Vis.Logic_Float := 0.07;

   ---------------------------------------------------------------------------
   --  Initializes the tree-layout-algorithm
   --
   --  Parameters (cp. specification 11.1.1.1: Treelayout / Parameter)
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes and edges to layout
   --      Edges are ignored
   --
   --    Widget:
   --      Graph_Widet where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock of the widget, released at the end of the layout
   --
   --    Target_Position:
   --      Position on window, where the middle of root-node has to
   --      be placed
   --
   --    Root:
   --      Root of the selection to be layouted
   --
   --    Meta_Class_Set_To_Layout:
   --      Meta_Class_Set containing node- and edge_classes to include in
   --      the tree-layout
   --      This Meta_Class_Set is NOT destroyed after usage, the caller has
   --        to take care for the unloading
   --
   --  Returns:
   --    derived Evolutions-Object to do the layout
   --
   --  Raises:
   --    Root_Node_Not_In_Selection Root_Node is not in
   --      Selection_To_Layout.Get_All_Nodes
   --
   --  Precondition:
   --    Root_Node is in Selection_To_Layout.Get_All_Nodes
   --
   function Initialize
     (Widget                   : in Graph_Widgets.Graph_Widget;
      Widget_Lock              : in Graph_Widgets.Lock_Type;
      Selection_To_Layout      : in Graph_Lib.Selections.Selection;
      Target_Position          : in Vis.Logic.Vector_2d;
      Root_Node                : in Graph_Lib.Node_Id;
      Meta_Class_Set_To_Layout : in Config.Class_Sets.Meta_Class_Set_Access)
     return Tree_Layout;

   -------------------
   --  Calculation  --
   -------------------

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Tree_Layout_Record;
      Canceled : in     Boolean);

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

   ---------------------------------------------------------------------------
   procedure Synchronized_Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

private
   ---------------------------------------------------------------------------
   type Layout_State is (Init_Start,
                         Init_Run_Part_One,
                         Init_Run_Part_Two,
                         FirstWalk_Start,
                         FirstWalk_Run_Part_One,
                         FirstWalk_Run_Part_Two,
                         SecondWalk_Start,
                         SecondWalk_Run,
                         Matrix);

   ---------------------------------------------------------------------------
   type Node_Layout_Data_Record;
   type Node_Layout_Data is access Node_Layout_Data_Record;
   type Node_Layout_Data_Record is record
      Thread            : Node_Layout_Data;

      --  by algorithm
      Ancestor          : Node_Layout_Data;

      --  by init-routine
      Parent            : Node_Layout_Data;

      --  = self, if self is leftmost silbling
      Leftmost_Silbling : Node_Layout_Data;

      -- used in the algorithm, null if n/a
      Left_Silbling     : Node_Layout_Data;

      --  for checking, if v is a silbling of w
      --  null if n/a
      Right_Silbling    : Node_Layout_Data;

      Leftmost_Child    : Node_Layout_Data;

      Rightmost_Child   : Node_Layout_Data;

      Silbling_Number   : Positive;

      Modf              : Vis.Logic_Float;
      Prelim            : Vis.Logic_Float;
      Change            : Vis.Logic_Float;
      Shift             : Vis.Logic_Float;

      --  Level in the tree
      Level             : Positive;

      --  Node in graph_lib
      Node              : Graph_Lib.Node_Id;
   end record;

   ---------------------------------------------------------------------------
   package Node_Layout_Data_Lists is new
     Lists (ItemType => Node_Layout_Data);

   ---------------------------------------------------------------------------
   type FirstWalk_Part_Two_Data_Record is record
      W               : Node_Layout_Data;
      DefaultAncestor : Node_Layout_Data;
   end record;

   ---------------------------------------------------------------------------
   package Node_Layout_Data_Stacks is new
     Stacks_Unbounded (Elem_Type => Node_Layout_Data);

   ---------------------------------------------------------------------------
   package FirstWalk_Part_Two_Stacks is new
     Stacks_Unbounded (Elem_Type => FirstWalk_Part_Two_Data_Record);

   ---------------------------------------------------------------------------
   type SecondWalk_Data_Record is record
      V               : Node_Layout_Data;
      M               : Vis.Logic_Float;
   end record;

   ---------------------------------------------------------------------------
   package SecondWalk_Stacks is new
     Stacks_Unbounded (Elem_Type => SecondWalk_Data_Record);

   ---------------------------------------------------------------------------
   function Id (P : in Positive) return Positive;
   package Level_Mappings is new
     Hashed_Mappings
     (Key_Type   => Positive,
      Value_Type => Vis.Logic_Float,
      Hash       => Id);

   ---------------------------------------------------------------------------
   type Tree_Layout_Record is
     new Evolutions.Iterative_Evolution with record
        --  Init by Initialize
        Widget           : Graph_Widgets.Graph_Widget;
        Widget_Lock      : Graph_Widgets.Lock_Type;
        Nodes_To_Layout  : Graph_Lib.Node_Id_Set;
        Target_Position  : Vis.Logic.Vector_2d;
        Root_Node        : Graph_Lib.Node_Id;
        Meta_Class_Set   : Config.Class_Sets.Meta_Class_Set_Access;
        State            : Layout_State;

        ----------------------------------------
        --  Init by Step / Synchronized_Step  --
        ----------------------------------------

        --  Rootnode of the tree to layout
        Tree_Root        : Node_Layout_Data;

        --  Distance between two nodes; used at FirstWalk
        X_Distance       : Vis.Logic_Float;

        --  Maximum X-Coordinate used at tree
        --  Set at SecondWalk
        Max_X            : Vis.Logic_Float;

        --  Used at conversion of Nodes_To_Layout to Layout_Tree
        --  Queue_Last is for speed optimization
        --    since Node_Id_Lists.Last has O(n) and gets O(1) with this "hack"
        --
        --  Queue may never be empty during the usage
        Queue_Last       : Node_Layout_Data_Lists.List;
        Queue            : Node_Layout_Data_Lists.List;

        Level_Heights              : Level_Mappings.Mapping;

        FirstWalk_Part_One_Stack   : Node_Layout_Data_Stacks.Stack;
        FirstWalk_Part_Two_Stack   : FirstWalk_Part_Two_Stacks.Stack;

        SecondWalk_Stack           : SecondWalk_Stacks.Stack;
     end record;

   ---------------------------------------------------------------------------
   function Are_Silblings
     (First  : in Node_Layout_Data;
      Second : in Node_Layout_Data)
     return Boolean;

end Giant.Tree_Layouts;
