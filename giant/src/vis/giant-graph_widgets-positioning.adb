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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets-positioning.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/07/07 03:35:59 $
--
------------------------------------------------------------------------------


with Ada.Numerics.Generic_Elementary_Functions;

with Giant.Graph_Widgets.Drawing;

package body Giant.Graph_Widgets.Positioning is


   use Vis.Absolute;
   use Vis.Logic;


   package Trigonometry is new Ada.Numerics.Generic_Elementary_Functions
     (Float_Type => Float);


   ------------------
   -- Calculations --
   ------------------

   --  Precondition:
   --    Get_X (Direction) /= 0.0
   function Calculate_Docking_Point_Not_Vertical
     (Direction : in     Vis.Logic.Vector_2d;
      Reversing : in     Boolean;
      Extent    : in     Vis.Absolute.Rectangle_2d;
      Spacing   : in     Vis.Absolute_Natural)
     return Vis.Absolute.Vector_2d is

      Sign          : Vis.Absolute_Int;
      X             : Vis.Absolute_Int;
      Y             : Vis.Absolute_Int;
      Point         : Vis.Absolute.Vector_2d;
      Gradient      : Float := Get_X (Direction) / Get_Y (Direction);
      Node_Gradient : Float := Vis.Logic_Float (Get_Width (Extent)) /
                                 Vis.Logic_Float (Get_Height (Extent));
   begin
      if abs Gradient <= Node_Gradient then
         --  intersection with vertical line
         if Get_X (Direction) > 0.0 xor Reversing then
            Sign := 1;
         else
            Sign := -1;
         end if;
         X := Get_Width (Extent) + Spacing;
         Y := Vis.Absolute_Int (Float (X) * Gradient);
         Point := Vis.Absolute.Get_Center (Extent) +
           Sign * Vis.Absolute.Combine_Vector (X, Y);
      else
         --  intersection with horizontal line
         if Get_Y (Direction) > 0.0 xor Reversing then
            Sign := 1;
         else
            Sign := -1;
         end if;
         Y := Get_Height (Extent) + Spacing;
         X := Vis.Absolute_Int (Float (Y) / Gradient);
         Point := Vis.Absolute.Get_Center (Extent) +
           Sign * Vis.Absolute.Combine_Vector (X, Y);
      end if;
      return Point;
   end Calculate_Docking_Point_Not_Vertical;

   function Calculate_Docking_Point_Vertical
      (Direction : in     Vis.Logic.Vector_2d;
       Reversing : in     Boolean;
       Extent    : in     Vis.Absolute.Rectangle_2d;
       Spacing   : in     Vis.Absolute_Natural)
      return Vis.Absolute.Vector_2d is
   begin
      if Get_Y (Direction) < 0.0 xor Reversing then
         return Get_Top_Center (Extent) -
           Vis.Absolute.Combine_Vector (0, Spacing);
      else
         return Get_Bottom_Center (Extent) +
           Vis.Absolute.Combine_Vector (0, Spacing);
      end if;
   end Calculate_Docking_Point_Vertical;

   procedure Calculate_Edge_Circle
     (Edge    : in     Vis_Data.Vis_Edge_Id;
      Center  : in     Vis.Absolute.Vector_2d;
      Spacing : in     Vis.Absolute_Natural;
      Radius  : in     Float) is

      Point            : Vis.Absolute.Vector_2d;
      Angle            : Float;
      Spacing_Angle    : Float :=
        Trigonometry.Arcsin (Float (Spacing) / Radius);
      Angle_Range      : Float := (2.0 * Ada.Numerics.Pi - Spacing_Angle) -
                                  (-Ada.Numerics.Pi + Spacing_Angle);
      Number_Of_Points : Natural := Vis_Data.Get_Number_Of_Points (Edge);
      Angle_Increment  : Float := Angle_Range / Float (Number_Of_Points - 1);
   begin
      Angle := -Ada.Numerics.Pi + Spacing_Angle;
      for Num in 1 .. Number_Of_Points loop
         Point := Center + Vis.Absolute.Combine_Vector
           (X => Vis.Absolute_Int (Radius * Trigonometry.Sin (Angle)),
            Y => Vis.Absolute_Int (Radius * Trigonometry.Cos (Angle)));
         Vis_Data.Set_Point (Edge, Num, Point);

         Angle := Angle + Angle_Increment;
      end loop;
   end Calculate_Edge_Circle;

   procedure Update_Loop_Position
     (Edge         : in     Vis_Data.Vis_Edge_Id;
      Dock_Spacing : in     Vis.Absolute_Natural) is

      Source        : Vis_Data.Vis_Node_Id := Vis_Data.Get_Source (Edge);
      Radius        : Float;
      Edge_Iterator : Vis_Data.Vis_Edge_Lists.ListIter;
      Current_Edge  : Vis_Data.Vis_Edge_Id;
   begin
      Radius := Default_Loop_Radius;
      Vis_Data.Make_Incoming_Iterator
        (Node           => Source,
         Incoming_Edges => Edge_Iterator);
      loop
         pragma Assert (Vis_Data.Vis_Edge_Lists.More (Edge_Iterator));
         Vis_Data.Vis_Edge_Lists.Next (Edge_Iterator, Current_Edge);
         exit when Vis_Data."=" (Current_Edge, Edge);
         if Vis_Data.Is_Loop (Current_Edge) then
            Radius := Radius + Default_Loop_Separation +
              Float (Vis_Data.Get_Thickness (Current_Edge));
         end if;
      end loop;

      Calculate_Edge_Circle
        (Edge    => Edge,
         Center  => Get_Bottom_Right (Vis_Data.Get_Extent (Source)),
         Spacing => Dock_Spacing,
         Radius  => Radius);
   end Update_Loop_Position;


   ----------------
   -- Life cycle --
   ----------------

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level) is
   begin
      Set_Zoom (Widget, Zoom);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      --  nothing to be done.
      null;
   end Shut_Down;

   procedure Set_Zoom
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level) is
   begin
      Widget.Positioning.Transformation := Vis.Get_Transformation
        (Origin => Vis.Logic.Zero_2d,
         Zoom   => Zoom);
   end Set_Zoom;


   procedure Update_Node_Position
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Source : Vis.Absolute.Vector_2d;
      Target : Vis.Absolute.Vector_2d;
   begin
      Source := Drawing.Get_Node_Border_Top_Center (Widget, Node);
      Target := Vis.Transform
        (Point          => Vis_Data.Get_Position (Node),
         Transformation => Widget.Positioning.Transformation);
      Vis_Data.Move_Node (Node, Target - Source);
   end Update_Node_Position;

   procedure Update_Edge_Position
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Dock_Spacing  : Vis.Absolute_Natural :=
        Default_Dock_Spacing + (Vis_Data.Get_Thickness (Edge) + 1) / 2;
      Source        : Vis_Data.Vis_Node_Id;
      Target        : Vis_Data.Vis_Node_Id;
      Source_Extent : Vis.Absolute.Rectangle_2d;
      Target_Extent : Vis.Absolute.Rectangle_2d;
      Source_Center : Vis.Logic.Vector_2d;
      Target_Center : Vis.Logic.Vector_2d;
      Direction     : Vis.Logic.Vector_2d;
      Start_Point   : Vis.Absolute.Vector_2d;
      End_Point     : Vis.Absolute.Vector_2d;
   begin
      if Vis_Data.Is_Loop (Edge) then
         Update_Loop_Position (Edge, Dock_Spacing);
      else
         Source := Vis_Data.Get_Source (Edge);
         Target := Vis_Data.Get_Target (Edge);
         Source_Extent := Vis_Data.Get_Extent (Source);
         Target_Extent := Vis_Data.Get_Extent (Target);
         Source_Center := Vis.Logic.Get_Center (Vis.To_Logic (Source_Extent));
         Target_Center := Vis.Logic.Get_Center (Vis.To_Logic (Target_Extent));
         Direction := Target_Center - Source_Center;

         if Get_X (Direction) /= 0.0 then
            Start_Point := Calculate_Docking_Point_Not_Vertical
              (Direction => Direction,
               Reversing => False,
               Extent    => Source_Extent,
               Spacing   => Dock_Spacing);

            End_Point := Calculate_Docking_Point_Not_Vertical
              (Direction => Direction,
               Reversing => True,
               Extent    => Target_Extent,
               Spacing   => Dock_Spacing);
         else
            Start_Point := Calculate_Docking_Point_Vertical
              (Direction => Direction,
               Reversing => False,
               Extent    => Source_Extent,
               Spacing   => Dock_Spacing);

            End_Point := Calculate_Docking_Point_Vertical
              (Direction => Direction,
               Reversing => True,
               Extent    => Target_Extent,
               Spacing   => Dock_Spacing);
         end if;

         Vis_Data.Set_Point
           (Edge  => Edge,
            Num   => 1,
            Point => Start_Point);
         Vis_Data.Set_Point
           (Edge  => Edge,
            Num   => Vis_Data.Get_Number_Of_Points (Edge),
            Point => End_Point);
      end if;
   end Update_Edge_Position;

   procedure Adjust_Ports
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is
   begin
      null;
   end Adjust_Ports;

   procedure Adjust_Arrow
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Point_Count  : Natural := Vis_Data.Get_Number_Of_Points (Edge);
      Source_Point : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, Point_Count - 1);
      Target_Point : Vis.Absolute.Vector_2d :=
        Vis_Data.Get_Point (Edge, Point_Count);
      Distance     : Vis.Absolute.Vector_2d := Target_Point - Source_Point;
      Angle        : Float;
      Left_Angle   : Float;
      Right_Angle  : Float;
      Left_Line    : Vis.Logic.Vector_2d;
      Right_Line   : Vis.Logic.Vector_2d;
   begin
      if Distance = Vis.Absolute.Zero_2d then
         Vis_Data.Set_Left_Arrow_Point (Edge, Target_Point);
         Vis_Data.Set_Right_Arrow_Point (Edge, Target_Point);
      else
         Angle := Trigonometry.Arctan
           (Float (Get_Y (Distance)), Float (Get_X (Distance)));
         Left_Angle := Angle + Default_Edge_Arrow_Angle;
         Right_Angle := Angle - Default_Edge_Arrow_Angle;

         Left_Line := Vis.Logic.Combine_Vector
           (X => Default_Edge_Arrow_Length * Trigonometry.Cos (Left_Angle),
            Y => Default_Edge_Arrow_Length * Trigonometry.Sin (Left_Angle));
         Right_Line := Vis.Logic.Combine_Vector
           (X => Default_Edge_Arrow_Length * Trigonometry.Cos (Right_Angle),
            Y => Default_Edge_Arrow_Length * Trigonometry.Sin (Right_Angle));

         Vis_Data.Set_Left_Arrow_Point
           (Edge,
            Target_Point - Vis.To_Absolute (Left_Line));
         Vis_Data.Set_Right_Arrow_Point
           (Edge,
            Target_Point - Vis.To_Absolute (Right_Line));
      end if;
   end Adjust_Arrow;

end Giant.Graph_Widgets.Positioning;
