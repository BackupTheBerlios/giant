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
--  $RCSfile: giant-matrix_layouts.adb,v $, $Revision: 1.4 $
--  $Author: koppor $
--  $Date: 2003/07/03 01:15:32 $
--

with Ada.Numerics.Generic_Elementary_Functions;

with Giant.Graph_Lib;

package body Giant.Matrix_Layouts is

   ---------------------------------------------------------------------------
   function Initialize
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d)
     return Matrix_Layout
   is
      Res : Matrix_Layout;
   begin
      Res                 := new Matrix_Layout_Record;
      Res.Widget          := Widget;
      Res.Widget_Lock     := Widget_Lock;
      Res.Nodes_To_Layout := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position := Target_Position;
      Res.State           := Init;

      --  Evolutions.Initialize
      Initialize
        (Individual => Res,
         Complexity => Graph_Lib.Node_Id_Sets.Size (Res.Nodes_To_Layout));

      return Res;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Matrix_Layout_Record;
      Canceled : in     Boolean)
   is
   begin
      Graph_Widgets.Release_Lock (Layout.Widget, Layout.Widget_Lock);
   end Finish;

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Matrix_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is

      ----------------------------------------------------------------------
      --  Removes top node of Layout.Nodes_To_Layout
      --  and sets its position to given pos
      procedure Set_Position_Of_Top_Node
        (Pos : in Vis.Logic.Vector_2d)
      is
         Node : Graph_Lib.Node_Id;
      begin
         Graph_Lib.Node_Id_Sets.Remove_First
           (Layout.Nodes_To_Layout, Node);
         Graph_Widgets.Set_Top_Middle
           (Layout.Widget,
            Node,
            Pos,
            Layout.Widget_Lock);
      end Set_Position_Of_Top_Node;

      procedure Init_Calculation
      is
        package Float_Functions is new
          Ada.Numerics.Generic_Elementary_Functions (Float_Type => Float);
      begin
         Layout.Matrix_Width := Natural
           (Float_Functions.Sqrt
            (Float (Graph_Lib.Node_Id_Sets.Size
                    (Layout.Nodes_To_Layout))));
         Layout.Current_Column     := 1;
         Layout.Current_Position   := Layout.Target_Position;
         Layout.Current_Row_Height := 0.0;
         Layout.Max_Node_Width     :=
           Graph_Widgets.Get_Current_Maximum_Node_Width (Layout.Widget);
      end Init_Calculation;

      procedure Do_Calculation
      is

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

         function Max
           (A : in Natural;
            B : in Natural)
           return Natural
         is
         begin
            if A > B then
               return A;
            else
               return B;
            end if;
         end Max;

         Number_Nodes_To_Layout : Natural;

      begin
         Number_Nodes_To_Layout := Min
           (Layout.Matrix_Width - Layout.Current_Column + 1,
            Max_Nodes_In_One_Run);
         Number_Nodes_To_Layout := Min
           (Number_Nodes_To_Layout,
            Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout));

         for I in Layout.Current_Column..Number_Nodes_To_Layout loop
            --  TBD: find next node by a breadth-first-search
            --       (according to spec)
            Set_Position_Of_Top_Node (Layout.Current_Position);

            Layout.Current_Column := Layout.Current_Column + 1;
            Vis.Logic.Set_X
              (Layout.Current_Position,
               Vis.Logic.Get_X (Layout.Current_Position) +
               Layout.Max_Node_Width +
               X_Distance);
         end loop;

         if Layout.Current_Column > Layout.Matrix_Width then
            --  we have reached EOL, go to beginning of line
            Layout.Current_Column := 1;
            Layout.Current_Position :=
              Vis.Logic.Combine_Vector
              (Vis.Logic.Get_X (Layout.Target_Position),
               Vis.Logic.Get_Y (Layout.Current_Position) +
               Layout.Current_Row_Height);
            Layout.Current_Row_Height := 0.0;
         end if;

         Evolutions.Advance_Progress (Layout, Number_Nodes_To_Layout);

         if Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) = 0 then
            Next_Action := Evolutions.Finish;
         else
            Next_Action := Evolutions.Run;
         end if;
      end Do_Calculation;

   begin
      case Layout.State is
         when Init =>
            case Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) is
               when 0 =>
                  Next_Action := Evolutions.Finish;
               when 1 =>
                  Set_Position_Of_Top_Node (Layout.Target_Position);
                  Next_Action := Evolutions.Finish;
               when others =>
                  Init_Calculation;
                  Layout.State := Calc;
                  Next_Action  := Evolutions.Run;
            end case;
         when Calc =>
            Do_Calculation;
      end case;
   end Step;

end Giant.Matrix_Layouts;

