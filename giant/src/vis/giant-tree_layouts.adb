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
--  $RCSfile: giant-tree_layouts.adb,v $, $Revision: 1.4 $
--  $Author: koppor $
--  $Date: 2003/07/04 17:32:55 $
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
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d;
      Root_Node           : in Giant.Graph_Lib.Node_Id)
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
   begin
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
      --  Inits the Init_Calculation, so that a break in the initialization
      --    is possible
      procedure Init_Calculation_Start
      is
      begin
         Graph_Lib.Node_Id_Sets.Remove
           (Layout.Nodes_To_Layout, Layout.Root_Node);

         Layout.Layout_Queue :=
           Graph_Lib.Node_Id_Lists.MakeList (Layout.Root_Node);
         Layout.Layout_Queue_Last := Layout.Layout_Queue;

         Layout.State := Init_Run;
         Next_Action  := Evolutions.Synchronize;
      end Init_Calculation_Start;

      ------------------------------------------------------------------------
      --  Converts Nodes_To_Layout to Layout_Tree
      procedure Init_Calculation_Run
      is
      begin
         --  here should be the real initialization
         --  remaining questions:
         --    * how
         --    * how to deal with the root, and the mapping
         --      node-id to node_layout_data, getting the children etc.
         Layout.State := FirstWalk;
         Next_Action  := Evolutions.Run;
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

