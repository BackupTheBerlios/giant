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
--  $RCSfile: giant-tree_layouts.adb,v $, $Revision: 1.5 $
--  $Author: koppor $
--  $Date: 2003/07/07 11:23:54 $
--
------------------------------------------------------------------------------
--  Variables are named according to the paper
--    http://citeseer.nj.nec.com/buchheim02improving.html
--    Revised version at http://www.zaik.uni-koeln.de/~paper/preprints.html
--      ?show=zaik2002-431&preprint_session=877c83a63d7134d8123dd3e6dd0ab004

with Giant.Matrix_Layouts;

package body Giant.Tree_Layouts is

   ---------------------------------------------------------------------------
   function Initialize
     (Widget                   : in Graph_Widgets.Graph_Widget;
      Widget_Lock              : in Graph_Widgets.Lock_Type;
      Selection_To_Layout      : in Graph_Lib.Selections.Selection;
      Target_Position          : in Vis.Logic.Vector_2d;
      Root_Node                : in Graph_Lib.Node_Id;
      Meta_Class_Set_To_Layout : in Config.Class_Sets.Meta_Class_Set_Access)
     return Tree_Layout
   is
      Res : Tree_Layout;
   begin
      if not Graph_Lib.Node_Id_Sets.Is_Member
        (Graph_Lib.Selections.Get_All_Nodes (Selection_To_Layout),
         Root_Node) then
         raise Root_Node_Not_In_Selection;
      end if;

      Res                 := new Tree_Layout_Record;
      Res.Widget          := Widget;
      Res.Widget_Lock     := Widget_Lock;
      Res.Nodes_To_Layout := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position := Target_Position;
      Res.Root_Node       := Root_Node;
      Res.Meta_Class_Set  := Meta_Class_Set_To_Layout;
      Res.State           := Init_Start;

      --  Evolutions.Initialize
      --  TBD: complexity, can be estimated, but I don't know how by now
      Initialize (Res);

      return Res;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Tree_Layout_Record;
      Canceled : in     Boolean)
   is

      -----------------------------------------------------------------------
      procedure Remove_Node_Layout_Data is
      begin
         --  TBD: recusively process Layout_Tree_Root and remove content
         --  out of the memory
         null;
      end Remove_Node_Layout_Data;

   begin
      if Canceled then
         case Layout.State is
            when others =>
               --  TBD
               null;
         end case;
      else
         Remove_Node_Layout_Data;
      end if;

      Graph_Widgets.Release_Lock (Layout.Widget, Layout.Widget_Lock);
   end Finish;

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      procedure Start_Matrix_Layout
      is
         The_Matrix_Layout : Matrix_Layouts.Matrix_Layout;
         The_Selection     : Graph_Lib.Selections.Selection;
      begin
         --  Create selection used as parameter for the layout
         --  a speed-optimization could be done here
         --  Selections.Create_Temp_Selection (Set, Set)
         --    which creates a wrapper-selection, which doesn't
         --    copy given sets and which doesn't destroy them either
         --    at Destroy
         The_Selection := Graph_Lib.Selections.Create ("tmp");
         Graph_Lib.Selections.Add_Node_Set
           (The_Selection, Layout.Nodes_To_Layout);

         The_Matrix_Layout := Matrix_Layouts.Initialize
           (Layout.Widget,
            Layout.Widget_Lock,
            The_Selection,
            Layout.Target_Position); --  TBD: modify to right corner of tree

         Graph_Lib.Selections.Destroy (The_Selection);

         --  do the matrix layout and return to here again
         Evolutions.Start_Sub_Calculation (Layout, The_Matrix_Layout);
         Next_Action := Evolutions.Finish;
      end Start_Matrix_Layout;

   begin
      case Layout.State is
         when Init_Start =>
            --  Depending on the amount of nodes to layout,
            --    we can do it now (0, 1) or we have to a non-trivial layout
            --    (>=2)
            case Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) is
               when 0 =>
                  Next_Action := Evolutions.Finish;
               when 1 =>
                  declare
                     Node : Graph_Lib.Node_Id;
                  begin
                     Graph_Lib.Node_Id_Sets.Remove_First
                       (Layout.Nodes_To_Layout, Node);
                     Graph_Widgets.Set_Top_Middle
                       (Layout.Widget,
                        Node,
                        Layout.Target_Position,
                        Layout.Widget_Lock);
                  end;
                  Next_Action := Evolutions.Finish;
               when others =>
                  --  Initialization can only be done in a synchronized
                  --    environment. With following statement, it is
                  --    jumped to there
                  Next_Action  := Evolutions.Synchronize;
            end case;
         when Init_Run =>
            Layout.State := FirstWalk;
            Next_Action  := Evolutions.Run;
         when FirstWalk =>
            Layout.State := SecondWalk;
            Next_Action  := Evolutions.Run;
         when SecondWalk =>
            Layout.State := Matrix;
            Next_Action  := Evolutions.Run;
         when Matrix =>
            Start_Matrix_Layout;
      end case;
   end Step;

   procedure Synchronized_Step
     (Layout      : access Tree_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      ------------------------------------------------------------------------
      --  Initializes one Node_Layout_Data-Item
      --
      --  Parameters:
      --    Node              : the belongig graph_lib-node
      --    Level             : states, to which level this node belongs to
      --    Leftmost_Silbling : The leftmost silbling of this node
      --                        null means, that the newly created node
      --                        is the leftmost silbing for itself
      function Generate_Node_Layout_Data
        (Node              : in Graph_Lib.Node_Id;
         Level             : in Positive;
         Leftmost_Silbling : in Node_Layout_Data := null)
        return Node_Layout_Data
      is
         Data : Node_Layout_Data;
      begin
         Data := new Node_Layout_Data_Record;
         --  from the paper
         Data.Modf     := 0;
         Data.Thread   := null;
         Data.Ancestor := Data;

         --  needed for the definitve layout
         Data.Level    := Level;
         Data.Node     := Node;

         --  build the necessary structure to know, where the node resides
         --    in the tree
         if Leftmost_Silbling = null then
            Data.Leftmost_Silbling := Data;
         else
            Data.Leftmost_Silbling := Leftmost_Silbling;
         end if;

         Data.Leftmost_Child := null;

         --  all others will be initialized later

         return Data;
      end Generate_Node_Layout_Data;

      ------------------------------------------------------------------------
      --  Inits the Init_Calculation, so that a break in the initialization
      --    is possible
      procedure Init_Calculation_Start
      is
      begin
         --  Root node is the first to be layouted
         Graph_Lib.Node_Id_Sets.Remove
           (Layout.Nodes_To_Layout, Layout.Root_Node);

         --  Generate layout-data for root node
         Layout.Layout_Tree_Root := Generate_Node_Layout_Data
           (Node  => Layout.Root_Node,
            Level => 1);

         --  Generate queue having root as first and only element
         Layout.Layout_Queue :=
           Node_Layout_Data_Lists.MakeList (Layout.Layout_Tree_Root);
         Layout.Layout_Queue_Last := Layout.Layout_Queue;

         --  next step is the "normal" initialize of the treelayout
         Layout.State := Init_Run;
         Next_Action  := Evolutions.Synchronize;
      end Init_Calculation_Start;

      ------------------------------------------------------------------------
      --  Converts Nodes_To_Layout to Layout_Tree
      procedure Init_Calculation_Run
      is

         ---------------------------------------------------------------------
         --  Creates Node_Layout_Data of all children of given node
         --  Children = all nodes reachable by parent and not yet having
         --             a respective Node_Layout_Data_Record
         --
         --  Nodes which are childs of Parent_Data will be removed from
         --    Layout.Nodes_To_Layous
         procedure Create_Children
           (Parent_Data : in Node_Layout_Data)
         is

            ------------------------------------------------------------------
            --  Appends given element to Layout.Layout_Queue in O(1)
            --
            --  a simple "Append" of Node_Layout_Data_Lists can't be used,
            --    since it needs O(n)
            procedure Append_To_Layout_Queue
              (Data : in Node_Layout_Data)
            is
               Last_Element : Node_Layout_Data_Lists.List;
            begin
               Last_Element := Node_Layout_Data_Lists.MakeList (Data);
               Node_Layout_Data_Lists.Attach
                 (Layout.Layout_Queue_Last, Last_Element);
               Layout.Layout_Queue_Last := Last_Element;
            end Append_To_Layout_Queue;

            --  Stores the current amount of Silblings
            Silbling_Count     : Natural;

            Outgoing_Edges     : Graph_Lib.Edge_Id_Array :=
              Graph_Lib.Get_Outgoing_Edges (Parent_Data.Node);

            --  indicates, if node was member in Layout.Nodes_To_Layout
            Found              : Boolean;

            --  holds the last created Node_Layout_Data
            Data               : Node_Layout_Data;

            Last_Silbling      : Node_Layout_Data;
            Leftmost_Silbling  : Node_Layout_Data;

            ----------------------------------------
            --  Data of the new Node_Layout_Data  --
            ----------------------------------------
            Node               : Graph_Lib.Node_Id;

         begin
            Silbling_Count    := 0;
            Last_Silbling     := null;
            Leftmost_Silbling := null;
            Data              := null;

            for I in Outgoing_Edges'Range loop
               --  TBD: add check for empty class set
               if Config.Class_Sets.Is_Edge_Class_Element_Of_Class_Set
                 (Layout.Meta_Class_Set,
                  Graph_Lib.Get_Edge_Class_Id (Outgoing_Edges (I))) then
                  Node := Graph_Lib.Get_Target_Node (Outgoing_Edges (I));
                  if Config.Class_Sets.Is_Node_Class_Element_Of_Class_Set
                    (Layout.Meta_Class_Set,
                     Graph_Lib.Get_Node_Class_Id (Node)) then
                     Graph_Lib.Node_Id_Sets.Remove_If_Exists
                       (Layout.Nodes_To_Layout, Node, Found);
                     if Found then
                        --  node is reachable, via a valid edge
                        --  is member of the class sets and is to be layouted

                        Silbling_Count := Silbling_Count + 1;

                        --  TBD: add/adjustcurrent_node_height to tree
                        --    of heights

                        Data := Generate_Node_Layout_Data
                          (Node, Parent_Data.Level, Leftmost_Silbling);
                        Data.Silbling_Number := Silbling_Count;

                        if Silbling_Count = 1 then
                           --  Data is the first child of the node
                           Leftmost_Silbling := Data;
                           Data.Leftmost_Silbling := Data;
                        else
                           Last_Silbling.Right_Silbling := Data;
                           Data.Left_Silbling := Last_Silbling;
                        end if;

                        Last_Silbling := Data;

                        --  the children of Data have to be created, too
                        --  --> append Data to queue
                        Append_To_Layout_Queue (Data);

                     end if;
                  end if;
               end if;
            end loop;

            --  there is no Data.Rightmost_Silbing, therefore
            --    there's no need to adjust Data, which
            --    is the Rightmost_Silbing now

            if Silbling_Count > 0 then
               --  there was at least one data created
               --  adjust child values of parent
               Parent_Data.Leftmost_Child  := Leftmost_Silbling;
               Parent_Data.Rightmost_Child := Data;
            end if;
         end Create_Children;

         ----------------------------------------------------------------------
         --  TBD: make separate package for this
         function Min
           (A : in Natural;
            B : in Natural)
           return Natural
         is
         begin
            if A < B then
               return A;
            else
               return B;
            end if;
         end Min;

         Current_Data            : Node_Layout_Data;
         Number_Nodes_To_Process : Natural;

      begin
         Number_Nodes_To_Process := Min
           (Node_Layout_Data_Lists.Length (Layout.Layout_Queue),
            Max_Nodes_In_One_Run);

         for I in 1..Number_Nodes_To_Process loop
            Current_Data := Node_Layout_Data_Lists.FirstValue
              (Layout.Layout_Queue);
            Create_Children (Current_Data);
            Node_Layout_Data_Lists.DeleteHead (Layout.Layout_Queue);
         end loop;

         Evolutions.Advance_Progress (Layout, Number_Nodes_To_Process);

         if Graph_Lib.Node_Id_Sets.Is_Empty (Layout.Nodes_To_Layout) or
           Node_Layout_Data_Lists.IsEmpty (Layout.Layout_Queue) then
            --  there are no more nodes to convert to Node_Layout_Data
            --    (i.e. in Layout.Layout_Queue are only leaves)
            --  or there are no more Node_Layout_Datas to process
            --  --> proceed to next step
            Layout.State := FirstWalk;
            Next_Action  := Evolutions.Run;
         else
            Next_Action := Evolutions.Synchronize;
         end if;
      end Init_Calculation_Run;

   begin
      case Layout.State is
         when Init_Start =>
            Init_Calculation_Start;
         when Init_Run =>
            Init_Calculation_Run;
         when others =>
            Next_Action  := Evolutions.Run;
      end case;
   end Synchronized_Step;

   ---------------------------------------------------------------------------
   function Are_Silblings
     (First  : in Node_Layout_Data;
      Second : in Node_Layout_Data)
     return Boolean
   is
   begin
      return (First.Leftmost_Silbling = Second.Leftmost_Silbling);
   end Are_Silblings;

end Giant.Tree_Layouts;

