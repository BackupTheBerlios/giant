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
--  $RCSfile: giant-tree_layouts.adb,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/07/03 15:59:35 $
--
------------------------------------------------------------------------------
--  Variables are named according to the paper
--    http://citeseer.nj.nec.com/buchheim02improving.html
--    Revised version at http://www.zaik.uni-koeln.de/~paper/preprints.html
--      ?show=zaik2002-431&preprint_session=877c83a63d7134d8123dd3e6dd0ab004

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
      Res                 := new Tree_Layout_Record;
      Res.Widget          := Widget;
      Res.Widget_Lock     := Widget_Lock;
      Res.Nodes_To_Layout := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position := Target_Position;
      Res.Root_Node       := Root_Node;
      Res.State           := Init;

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

      procedure Init_Calculation
      is
      begin
         null;
      end Init_Calculation;

   begin
      case Layout.State is
         when Init =>
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
                  Init_Calculation;
                  Layout.State := FirstWalk;
                  Next_Action  := Evolutions.Run;
            end case;
         when FirstWalk =>
            Layout.State := SecondWalk;
            Next_Action  := Evolutions.Run;
         when SecondWalk =>
            Next_Action := Evolutions.Finish;
      end case;
   end Step;

   ---------------------------------------------------------------------------
   function Are_Silblings
     (First  : in Node_Layout_Data_Access;
      Second : in Node_Layout_Data_Access)
     return Boolean
   is
      Temp_Node : Node_Layout_Data_Access;
   begin
      Temp_Node:=First.Leftmost_Silbling;
      while (Temp_Node /= null) and (Temp_Node /= Second) loop
         Temp_Node := Temp_Node.Right_Silbling;
      end loop;
      return Temp_Node/=null;
   end Are_Silblings;

end Giant.Tree_Layouts;

