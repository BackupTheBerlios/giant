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
--  $RCSfile: giant-vis_data.adb,v $, $Revision: 1.8 $
--  $Author: keulsn $
--  $Date: 2003/06/24 10:55:04 $
--
------------------------------------------------------------------------------


with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Giant.Logger;

package body Giant.Vis_Data is

   package Vis_Data_Logger is new Logger
     (Name => "Giant.Vis_Data");

   procedure Set_Min_Max
     (Value_1 : in     Vis.Absolute_Int;
      Value_2 : in     Vis.Absolute_Int;
      Min     :    out Vis.Absolute_Int;
      Max     :    out Vis.Absolute_Int) is
   begin
      if Value_1 <= Value_2 then
         Min := Value_1;
         Max := Value_2;
      else
         Max := Value_1;
         Min := Value_2;
      end if;
   end Set_Min_Max;


   ------------
   -- Layers --
   ------------

   function Is_Below
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean is
   begin
      return Low < High;
   end Is_Below;

   function Is_Below_Or_Equal
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean is
   begin
      return Low <= High;
   end Is_Below_Or_Equal;


   -----------------
   -- Layer_Pools --
   -----------------

   procedure Reset_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Bottom_Layer;
   end Reset_Pool;

   function Get_Highest_Layer
     (Pool : in     Layer_Pool)
     return Layer_Type is
   begin
      return Layer_Type (Pool);
   end Get_Highest_Layer;

   procedure Enlarge_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Pool + 1;
   end Enlarge_Pool;

   procedure Shrink_Pool
     (Pool : in out Layer_Pool) is
   begin
      Pool := Pool - 1;
   end Shrink_Pool;


   -----------
   -- Edges --
   -----------

   function Get_Layer
     (Edge  : in     Vis_Edge_Id)
     return Layer_Type is
   begin
      return Edge.Layer;
   end Get_Layer;

   function Get_Graph_Edge
     (Edge : in     Vis_Edge_Id)
     return Graph_Lib.Edge_Id is
   begin
      return Edge.Edge;
   end Get_Graph_Edge;

   function Get_Source
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id is
   begin
      return Edge.Source;
   end Get_Source;

   function Get_Target
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id is
   begin
      return Edge.Target;
   end Get_Target;

   function Get_Thickness
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute_Natural is
   begin
      return Edge.Thickness;
   end Get_Thickness;

   function Get_Number_Of_Points
     (Edge : in     Vis_Edge_Id)
     return Edge_Point_Number is
   begin
      return Edge.Points'Length;
   end Get_Number_Of_Points;

   function Get_Point
     (Edge : in     Vis_Edge_Id;
      Num  : in     Positive)
     return Vis.Absolute.Vector_2d is
   begin
      return Edge.Points (Num);
   end Get_Point;

   function Has_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Vis.Absolute.Get_Height (Edge.Text_Area) < Vis.Absolute_Int'Last;
   end Has_Text_Area;

   function Get_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Edge.Text_Area;
   end Get_Text_Area;

   function Is_Hidden
     (Edge : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Edge.Flags (Hidden);
   end Is_Hidden;

   function Get_Highlighting
     (Edge : in     Vis_Edge_Id)
     return Flags_Type is

      Only_Highlighting : Flags_Type := (Highlight_Type'Range => True,
                                         others => False);
   begin
      return Edge.Flags and Only_Highlighting;
   end Get_Highlighting;

   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Is_Below (Get_Layer (Left), Get_Layer (Right));
   end Is_Edge_Below;


   -----------
   -- Nodes --
   -----------

   function Get_Layer
     (Node  : in     Vis_Node_Id)
     return Layer_Type is
   begin
      return Node.Layer;
   end Get_Layer;

   function Get_Graph_Node
     (Node : in     Vis_Node_Id)
     return Graph_Lib.Node_Id is
   begin
      return Node.Node;
   end Get_Graph_Node;

   function Get_Top_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis.Absolute.Get_Top_Center (Node.Extent);
   end Get_Top_Center;

   function Get_Extent
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Node.Extent;
   end Get_Extent;

   procedure Make_Incoming_Iterator
     (Node           : in     Vis_Node_Id;
      Incoming_Edges :    out Vis_Edge_Sets.Iterator) is
   begin
      Incoming_Edges := Vis_Edge_Sets.Make_Iterator (Node.Incoming_Edges);
   end Make_Incoming_Iterator;

   procedure Make_Outgoing_Iterator
     (Node           : in     Vis_Node_Id;
      Outgoing_Edges :    out Vis_Edge_Sets.Iterator) is
   begin
      Outgoing_Edges := Vis_Edge_Sets.Make_Iterator (Node.Outgoing_Edges);
   end Make_Outgoing_Iterator;

   function Is_Hidden
     (Node : in     Vis_Node_Id)
     return Boolean is
   begin
      return Node.Flags (Hidden);
   end Is_Hidden;

   function Get_Highlighting
     (Node : in     Vis_Node_Id)
     return Flags_Type is

      Only_Highlighting : Flags_Type := (Highlight_Type'Range => True,
                                         others => False);
   begin
      return Node.Flags and Only_Highlighting;
   end Get_Highlighting;

   function Is_Node_Below
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean is
   begin
      return Is_Below (Get_Layer (Left), Get_Layer (Right));
   end Is_Node_Below;


   -------------
   -- Regions --
   -------------

   function Hash_Region_Position
     (Key : in Region_Position)
     return Integer is

      Vector : Vis.Absolute.Vector_2d := Vis.Absolute.Vector_2d (Key);
      X      : Vis.Absolute_Int       := abs Vis.Absolute.Get_X (Vector);
      Y      : Vis.Absolute_Int       := abs Vis.Absolute.Get_Y (Vector);
      Value  : Integer;
   begin
      -- use lower 2 bytes of each coordinate
      Value := 16#1# * (X mod 16#100#) + 16#100# * (Y mod 16#100#);
      X := X / 16#100#;
      Y := Y / 16#100#;
      Value := Value + 16#1_0000# * (X mod 16#100#)
        + 16#100_0000# * (Y mod 16#100#);
      return Value;
   end Hash_Region_Position;

   function Order_Position
     (Left  : in    Region_Position;
      Right : in    Region_Position)
     return Boolean is
   begin
      if Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Left)) =
        Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Right)) then

         return Vis.Absolute.Get_Y (Vis.Absolute.Vector_2d (Left)) <
           Vis.Absolute.Get_Y (Vis.Absolute.Vector_2d (Right));
      else
         return Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Left)) <
           Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Right));
      end if;
   end Order_Position;

   function Create_Region
     (Extent : Vis.Absolute.Rectangle_2d)
     return Region_Id is

      Region : Region_Id := new Region_Record;
   begin
      Region.Extent := Extent;
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
      Region.Background_Polluted := False;
      Region.Edges := Vis_Edge_Sets.Empty_Set;
      Region.Nodes := Vis_Node_Sets.Empty_Set;
      return Region;
   end Create_Region;

   procedure Destroy_Region
     (Region : in out Region_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Region_Record,
         Name   => Region_Id);

   begin
      if Region /= null then
         Vis_Edge_Sets.Destroy (Region.Edges);
         Vis_Node_Sets.Destroy (Region.Nodes);
         Free (Region);
      end if;
   end Destroy_Region;

   procedure Add_Background_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
      Region.Background_Polluted := True;
   end Add_Background_Pollution;

   procedure Add_Edge_Pollution
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is
   begin
      if not Region.Background_Polluted then
         if Region.Polluted_Edge /= null then
            if Is_Edge_Below (Edge, Region.Polluted_Edge) then
               Region.Polluted_Edge := Edge;
            end if;
         else
            Region.Polluted_Edge := Edge;
            Region.Polluted_Node := null;
         end if;
      end if;
   end Add_Edge_Pollution;

   procedure Add_Node_Pollution
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is
   begin
      if not Region.Background_Polluted
        and then Region.Polluted_Edge = null
        and then (Region.Polluted_Node = null or else
                  Is_Node_Below (Node, Region.Polluted_Node))
      then
         Region.Polluted_Node := Node;
      end if;
   end Add_Node_Pollution;

   procedure Remove_Background_Pollution
     (Region : access Region_Record) is
   begin
      if Region.Background_Polluted then
         Region.Background_Polluted := False;
         if not Vis_Edge_Sets.Is_Empty (Region.Edges) then
            Region.Polluted_Edge := Vis_Edge_Sets.First (Region.Edges);
         elsif not Vis_Node_Sets.Is_Empty (Region.Nodes) then
            Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
         end if;
      end if;
   end Remove_Background_Pollution;

   procedure Remove_Foreground_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Edge := null;
      Region.Polluted_Node := null;
   end Remove_Foreground_Pollution;

   procedure Remove_Edge_Pollution
     (Region : access Region_Record) is
   begin
      if Region.Polluted_Edge /= null then
         Region.Polluted_Edge := null;
         if not Vis_Node_Sets.Is_Empty (Region.Nodes) then
            Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
         end if;
      end if;
   end Remove_Edge_Pollution;

   procedure Remove_Node_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Node := null;
   end Remove_Node_Pollution;

   procedure Remove_All_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Node := null;
      Region.Polluted_Edge := null;
      Region.Background_Polluted := False;
   end Remove_All_Pollution;

   function Is_Background_Polluted
     (Region : access Region_Record)
     return Boolean is
   begin
      return Region.Background_Polluted;
   end Is_Background_Polluted;

   --  Iterator must be destroyed.
   function Get_Polluted_Edges
     (Region : access Region_Record)
     return Vis_Edge_Sets.Iterator is

      Iterator : Vis_Edge_Sets.Iterator;
   begin
      if Region.Polluted_Edge /= null then
         --  Start from first polluted edge
         Iterator := Vis_Edge_Sets.Make_Iterator_On_Element
           (Region.Edges, Region.Polluted_Edge);
      elsif Region.Background_Polluted then
         --  All edges are polluted
         Iterator := Vis_Edge_Sets.Make_Iterator (Region.Edges);
      else
         --  Make empty Iterator
         Iterator := Vis_Edge_Sets.Make_Reverse_Iterator (Region.Edges);
         if Vis_Edge_Sets.More (Iterator) then
            Vis_Edge_Sets.Next (Iterator);
         end if;
      end if;
      return Iterator;
   end Get_Polluted_Edges;

   --  Iterator must be destroyed.
   function Get_Polluted_Nodes
     (Region : access Region_Record)
     return Vis_Node_Sets.Iterator is

      Iterator : Vis_Node_Sets.Iterator;
   begin
      if Region.Polluted_Node /= null then
         --  Start at first polluted node
         Iterator := Vis_Node_Sets.Make_Iterator_On_Element
           (Region.Nodes, Region.Polluted_Node);
      elsif Region.Polluted_Edge /= null or Region.Background_Polluted then
         --  All nodes are polluted
         Iterator := Vis_Node_Sets.Make_Iterator (Region.Nodes);
      else
         --  Make empty iterator
         Iterator := Vis_Node_Sets.Make_Reverse_Iterator (Region.Nodes);
         if Vis_Node_Sets.More (Iterator) then
            Vis_Node_Sets.Next (Iterator);
         end if;
      end if;
      return Iterator;
   end Get_Polluted_Nodes;

   procedure Add_Node_To_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is
   begin
      pragma Assert (Node /= null);
      Vis_Node_Sets.Insert (Region.Nodes, Node);
   end Add_Node_To_Region;

   procedure Add_Edge_To_Region
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is
   begin
      pragma Assert (Edge /= null);
      Vis_Edge_Sets.Insert (Region.Edges, Edge);
   end Add_Edge_To_Region;

   procedure Remove_Edge_From_Region
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is

      Iterator : Vis_Edge_Sets.Iterator;
   begin
      pragma Assert (Edge /= null);
      if Edge = Region.Polluted_Edge then
         Iterator := Vis_Edge_Sets.Make_Iterator_On_Element
           (A_Set   => Region.Edges,
            Element => Edge);
         Vis_Edge_Sets.Next (Iterator);
         if Vis_Edge_Sets.More (Iterator) then
            Region.Polluted_Edge := Vis_Edge_Sets.Current (Iterator);
         else
            Region.Polluted_Edge := null;
            if not Vis_Node_Sets.Is_Empty (Region.Nodes) then
               Region.Polluted_Node := Vis_Node_Sets.First (Region.Nodes);
            end if;
         end if;
         Vis_Edge_Sets.Destroy (Iterator);
      end if;
      Vis_Edge_Sets.Remove_If_Exists
        (A_Set   => Region.Edges,
         Element => Edge);
   end Remove_Edge_From_Region;

   procedure Remove_Node_From_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is

      Iterator : Vis_Node_Sets.Iterator;
   begin
      pragma Assert (Node /= null);
      if Node = Region.Polluted_Node then
         Iterator := Vis_Node_Sets.Make_Iterator_On_Element
           (A_Set   => Region.Nodes,
            Element => Node);
         Vis_Node_Sets.Next (Iterator);
         if Vis_Node_Sets.More (Iterator) then
            Region.Polluted_Node := Vis_Node_Sets.Current (Iterator);
         else
            Region.Polluted_Node := null;
         end if;
         Vis_Node_Sets.Destroy (Iterator);
      end if;
      Vis_Node_Sets.Remove_If_Exists
        (A_Set   => Region.Nodes,
         Element => Node);
   end Remove_Node_From_Region;

   function Intersects_Area_Region
     (Region    : access Region_Record;
      Rectangle : in     Vis.Absolute.Rectangle_2d)
     return Boolean is
   begin
      return Vis.Absolute.Intersects (Region.Extent, Rectangle);
   end Intersects_Area_Region;

--     function Intersects_Edge_Region
--       (Region : access Region_Record;
--        Edge   : in     Vis_Edge_Id)
--       return Boolean is
--     begin
--     end Intersects_Edge_Region;

   function Intersects_Node_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id)
     return Boolean is
   begin
      return Vis.Absolute.Intersects (Region.Extent, Node.Extent);
   end Intersects_Node_Region;

   function Get_Region_Extent
     (Region : access Region_Record)
     return Vis.Absolute.Rectangle_2d is
   begin
      return Region.Extent;
   end Get_Region_Extent;


   ---------------------
   -- Region_Managers --
   ---------------------

   procedure Init_Region_Manager
     (Manager         :    out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural) is
   begin
      if Region_Mappings.Is_Empty (Manager.Regions) then
         Vis_Data_Logger.Debug ("Init_Region_Manager not implemented.");
      else
         Vis_Data_Logger.Debug ("Unnecessary call to Init_Region_Manager.");
      end if;
   end Init_Region_Manager;

   function Get_Position_X_At_X
     (Manager : in     Region_Manager;
      Point_X : in     Vis.Absolute_Int)
     return Vis.Absolute_Int is

      Position_X : Vis.Absolute_Int;
   begin
      Position_X := Point_X / Manager.Region_Width;
      if Point_X < 0 and then Point_X mod Manager.Region_Width /= 0 then
         Position_X := Position_X - 1;
      end if;
      return Position_X;
   end Get_Position_X_At_X;
   pragma Inline (Get_Position_X_At_X);

   function Get_Position_Y_At_Y
     (Manager : in     Region_Manager;
      Point_Y : in     Vis.Absolute_Int)
     return Vis.Absolute_Int is

      Position_Y : Vis.Absolute_Int := Point_Y / Manager.Region_Height;
   begin
      if Point_Y < 0 and then Point_Y mod Manager.Region_Height /= 0 then
         Position_Y := Position_Y - 1;
      end if;
      return Position_Y;
   end Get_Position_Y_At_Y;
   pragma Inline (Get_Position_Y_At_Y);


   function Get_Region_Position
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Region_Position is

      Position   : Region_Position;
      Point_X    : Vis.Absolute_Int := Vis.Absolute.Get_X (Point);
      Point_Y    : Vis.Absolute_Int := Vis.Absolute.Get_Y (Point);
      Position_X : Vis.Absolute_Int := Get_Position_X_At_X (Manager, Point_X);
      Position_Y : Vis.Absolute_Int := Get_Position_Y_At_Y (Manager, Point_Y);
   begin
      return Region_Position
        (Vis.Absolute.Combine_Vector (Position_X, Position_Y));
   end Get_Region_Position;

   function Get_Region_Extent
     (Manager  : in     Region_Manager;
      Position : in     Region_Position)
     return Vis.Absolute.Rectangle_2d is

      X : Vis.Absolute_Int := Vis.Absolute.Get_X
        (Vis.Absolute.Vector_2d (Position));
      Y : Vis.Absolute_Int := Vis.Absolute.Get_Y
        (Vis.Absolute.Vector_2d (Position));
   begin
      return Vis.Absolute.Combine_Rectangle
        (X * Manager.Region_Width,
         Y * Manager.Region_Height,
         (X + 1) * Manager.Region_Width - 1,
         (Y + 1) * Manager.Region_Height - 1);
   end Get_Region_Extent;

   --  Gets the region for a position. If that region does not exist yet,
   --  then null will be returned.
   function Get_Region_If_Exists
     (Manager  : in     Region_Manager;
      Position : in     Region_Position)
     return Region_Id is
   begin
      if Region_Mappings.Is_Bound (Manager.Regions, Position) then
         return Region_Mappings.Fetch (Manager.Regions, Position);
      else
         return null;
      end if;
   end Get_Region_If_Exists;

   --  Gets the region for a position. If that region does not exist yet then
   --  it will be created.
   procedure Get_Region
     (Manager  : in out Region_Manager;
      Position : in     Region_Position;
      Region   :    out Region_Id) is
   begin
      if Region_Mappings.Is_Bound (Manager.Regions, Position) then
         Region := Region_Mappings.Fetch (Manager.Regions, Position);
      else
         Region := Create_Region (Get_Region_Extent (Manager, Position));
         Region_Mappings.Bind (Manager.Regions, Position, Region);
      end if;
   end Get_Region;

   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d) is

      Top_Left_Position     : Region_Position;
      Bottom_Right_Position : Region_Position;
   begin
      --  Find top-left and bottom-right regions
      Top_Left_Position := Get_Region_Position
        (Manager, Vis.Absolute.Get_Top_Left (Area));
      Bottom_Right_Position := Get_Region_Position
        (Manager, Vis.Absolute.Get_Bottom_Right (Area));
      --  Take top-left and bottom-right points of these regions
      Area := Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Get_Top_Left
                           (Get_Region_Extent
                            (Manager, Top_Left_Position)),
         Bottom_Right => Vis.Absolute.Get_Bottom_Right
                           (Get_Region_Extent
                            (Manager, Bottom_Right_Position)));
   end Optimize_Drawing_Area;

   generic
      with procedure Hit
        (Position : in     Region_Position);
   procedure Add_Lines_Positions
     (Manager     : in     Region_Manager;
      Thickness   : in     Vis.Absolute_Natural;
      Start_Point : in     Vis.Absolute.Vector_2d;
      End_Point   : in     Vis.Absolute.Vector_2d);

   procedure Add_Lines_Positions
     (Manager     : in     Region_Manager;
      Thickness   : in     Vis.Absolute_Natural;
      Start_Point : in     Vis.Absolute.Vector_2d;
      End_Point   : in     Vis.Absolute.Vector_2d) is

      use Vis.Absolute;
      use Vis.Logic;

      procedure Add_Line_Bottom_To_Top
        (Starting_Point : in     Vis.Absolute.Vector_2d;
         Ending_Point   : in     Vis.Absolute.Vector_2d) is

         Left_To_Right : constant Boolean :=
           Get_X (Starting_Point) <= Get_X (End_Point);

         function Get_Outer_Position_X
           (Point_X            : in     Vis.Logic_Float;
            Horizontal_Profile : in     Vis.Logic_Float)
           return Vis.Absolute_Int is

            X : Vis.Absolute_Int;
         begin
            if Left_To_Right then
               X := Vis.Absolute_Int (Point_X - Horizontal_Profile);
            else
               X := Vis.Absolute_Int (Point_X + Horizontal_Profile + 0.5);
            end if;
            return Get_Position_X_At_X (Manager, X);
         end Get_Outer_Position_X;

         function Get_Inner_Position_X
           (Point_X            : in     Vis.Logic_Float;
            Horizontal_Profile : in     Vis.Logic_Float)
           return Vis.Absolute_Int is

            X : Vis.Absolute_Int;
         begin
            if Left_To_Right then
               X := Vis.Absolute_Int (Point_X + Horizontal_Profile + 0.5);
            else
               X := Vis.Absolute_Int (Point_X - Horizontal_Profile);
            end if;
            return Get_Position_X_At_X (Manager, X);
         end Get_Inner_Position_X;

         package Numerics is new Ada.Numerics.Generic_Elementary_Functions
           (Float_Type => Vis.Logic_Float);

         Line_Start            : Vis.Logic.Vector_2d;
         Difference            : Vis.Logic.Vector_2d;
         Alpha                 : Vis.Logic_Float;
         Horizontal_Profile    : Vis.Logic_Float;
         Min_Y                 : Vis.Absolute_Int;
         Max_Y                 : Vis.Absolute_Int;
         Done                  : Boolean;
         Current_Y             : Vis.Absolute_Int;
         X                     : Vis.Logic_Float;
         Inner_Position_X      : Vis.Absolute_Int;
         Outer_Position_X      : Vis.Absolute_Int;
         Position              : Region_Position;
         Min_Position_X        : Vis.Absolute_Int;
         Max_Position_X        : Vis.Absolute_Int;
      begin
         pragma Assert (Get_Y (Starting_Point) >= Get_Y (Ending_Point));
         Line_Start := Vis.To_Logic (Starting_Point);
         Difference := Vis.To_Logic (Ending_Point - Starting_Point);

         Alpha := Numerics.Arctan (Get_Y (Difference), Get_X (Difference));

         Horizontal_Profile := Vis.To_Logic_Float ((Thickness + 3) / 2)
           / abs Numerics.Sin (Alpha);

         Min_Y := Get_Y (Ending_Point) - (Thickness + 3) / 2;
         Max_Y := Get_Y (Starting_Point) + (Thickness + 3) / 2;

         --  Process regions line by line, starting at bottom then ascending
         --  to top

         --  use 'Position' to keep track of the current region's position
         Set_Y (Position, Get_Position_Y_At_Y (Manager, Max_Y));
         --  'Current_Y' reflects the horizontal lines directly below the
         --  starting point, then above the region at Position and at last
         --  directly above the end point
         Current_Y := Max_Y;
         --  'X' is the horizontal intersection coordinate of the line
         --  with the horizontal line at 'Current_Y'
         X := Vis.Intersects_Line_Horizontal_Line_X
           (Origin     => Line_Start,
            Direction  => Difference,
            Horizontal => Vis.Logic_Float (Current_Y) - 0.5);
         --  the drawn line intersects the horizontal line at 'Current_Y'
         --  between the position-columns 'Outer_Position_X'
         --  and 'Inner_Position_X'
         Outer_Position_X := Get_Outer_Position_X (X, Horizontal_Profile);
         Done := False;
         loop
            --  next horizontal line
            Current_Y := Get_Top (Get_Region_Extent (Manager, Position));
            --  above ending point?
            if Current_Y <= Min_Y then
               --  if yes, set directly above point and finish
               Current_Y := Min_Y;
               Done := True;
            end if;
            --  intersection with the new 'Current_Y'
            X := Vis.Intersects_Line_Horizontal_Line_X
              (Origin     => Line_Start,
               Direction  => Difference,
               Horizontal => Vis.Logic_Float (Current_Y) - 0.5);
            --  update 'Inner_Position_X' only. 'Outer_Position_X' is taken
            --  from the previous iteration (at the previous 'Current_Y')
            Inner_Position_X := Get_Inner_Position_X (X, Horizontal_Profile);

            --  add all positions between 'Outer_Position_X' and
            --  'Inner_Position_X'
            Set_Min_Max
              (Value_1 => Inner_Position_X,
               Value_2 => Outer_Position_X,
               Min     => Min_Position_X,
               Max     => Max_Position_X);
            for Pos_X in Min_Position_X .. Max_Position_X loop
               Set_X (Position, Pos_X);
               Hit (Position);
            end loop;

            --  Exit if top reached
            exit when Done;

            --  Update 'Outer_Position_X' to the current 'Current_Y'
            Outer_Position_X := Get_Outer_Position_X (X, Horizontal_Profile);
            --  Step one line of regions higher
            Set_Y (Position, Get_Y (Position) - 1);
         end loop;
      end Add_Line_Bottom_To_Top;

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Area     : Vis.Absolute.Rectangle_2d;
      Min_Y    : Vis.Absolute_Int;
      Max_Y    : Vis.Absolute_Int;
   begin
      if Vis.Absolute.Get_X (Start_Point) = Vis.Absolute.Get_X (End_Point) then
         --  Vertical line can be treated as rectangle
         Vis.Absolute.Set_Left
           (Area, Vis.Absolute.Get_X (Start_Point) - (Thickness + 1) / 2);
         Vis.Absolute.Set_Right
           (Area, Vis.Absolute.Get_X (Start_Point) + (Thickness + 1) / 2);
         Set_Min_Max
           (Value_1 => Vis.Absolute.Get_Y (Start_Point),
            Value_2 => Vis.Absolute.Get_Y (End_Point),
            Min     => Min_Y,
            Max     => Max_Y);
         Vis.Absolute.Set_Top (Area, Min_Y - (Thickness + 1) / 2);
         Vis.Absolute.Set_Bottom (Area, Max_Y + (Thickness + 1) / 2);

         Pool := Create_Position_Pool_From_Area
           (Manager, Combine_Rectangle (Start_Point, End_Point));
         Make_Position_Iterator (Pool, Iterator);
         while Has_More (Iterator) loop
            Hit (Get_Current (Iterator));
            Next (Iterator);
         end loop;

      else
         if Vis.Absolute.Get_Y (Start_Point) <=
           Vis.Absolute.Get_Y (End_Point) then

            Add_Line_Bottom_To_Top (Start_Point, End_Point);
         else
            Add_Line_Bottom_To_Top (End_Point, Start_Point);
         end if;
      end if;
   end Add_Lines_Positions;

   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Positions : Position_Sets.Set := Position_Sets.Empty_Set;

      procedure Hit_Position
        (Position : in     Region_Position) is

         Inserted  : Boolean;
         Region    : Region_Id;
      begin
         Position_Sets.Insert
           (A_Set   => Positions,
            Element => Position,
            Is_New  => Inserted);

         if Inserted then
            Get_Region
              (Manager  => Manager,
               Position => Position,
               Region   => Region);

            Region_Lists.Attach (Region, Edge.Regions);
            Add_Edge_To_Region (Region, Edge);
            Add_Edge_Pollution (Region, Edge);
         end if;
      end Hit_Position;

      procedure Add_Edge_Line_Positions is new Add_Lines_Positions
        (Hit => Hit_Position);

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
   begin
      for I in Edge.Points'First .. Edge.Points'Last - 1 loop
         Add_Edge_Line_Positions
           (Manager     => Manager,
            Thickness   => Edge.Thickness,
            Start_Point => Edge.Points (I),
            End_Point   => Edge.Points (I + 1));
      end loop;
      Add_Edge_Line_Positions
        (Manager     => Manager,
         Thickness   => Edge.Thickness,
         Start_Point => Edge.Left_Arrow_Point,
         End_Point   => Edge.Points (Edge.Points'Last));
      Add_Edge_Line_Positions
        (Manager     => Manager,
         Thickness   => Edge.Thickness,
         Start_Point => Edge.Right_Arrow_Point,
         End_Point   => Edge.Points (Edge.Points'Last));

      if Has_Text_Area (Edge) then
         Pool := Create_Position_Pool_From_Area (Manager, Edge.Text_Area);
         Make_Position_Iterator (Pool, Iterator);
         while Has_More (Iterator) loop
            Hit_Position (Get_Current (Iterator));
         end loop;
      end if;

      Position_Sets.Destroy (Positions);
   end Insert_Edge;

   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Region : Region_Id;
   begin
      while not Region_Lists.IsEmpty (Edge.Regions) loop
         Region := Region_Lists.FirstValue (Edge.Regions);
         Remove_Edge_From_Region (Region, Edge);
         Add_Background_Pollution (Region);
         Region_Lists.DeleteHead (Edge.Regions);
      end loop;
   end Drop_Edge;

   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Region   : Region_Id;
   begin
      Pool := Create_Position_Pool_From_Area (Manager, Get_Extent (Node));
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Get_Region
           (Manager  => Manager,
            Position => Get_Current (Iterator),
            Region   => Region);
         Next (Iterator);
         Region_Lists.Attach (Region, Node.Regions);
         Add_Node_To_Region (Region, Node);
         Add_Node_Pollution (Region, Node);
      end loop;
   end Insert_Node;

   procedure Drop_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Region : Region_Id;
   begin
      while not Region_Lists.IsEmpty (Node.Regions) loop
         Remove_Node_From_Region
           (Region_Lists.FirstValue (Node.Regions), Node);
         Region_Lists.DeleteHead
           (Node.Regions);
         Region := Region_Lists.FirstValue (Node.Regions);
         Remove_Node_From_Region (Region, Node);
         Add_Background_Pollution (Region);
         Region_Lists.DeleteHead (Node.Regions);
      end loop;
   end Drop_Node;

   procedure Pollute_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is

      Iterator       : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Edge.Regions);
      Current_Region : Region_Id;
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.CellValue (Iterator);
         Add_Edge_Pollution (Current_Region, Edge);
         Region_Lists.Forward (Iterator);
      end loop;
   end Pollute_Edge;

   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Iterator       : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Node.Regions);
      Current_Region : Region_Id;
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.CellValue (Iterator);
         Add_Node_Pollution (Current_Region, Node);
         Region_Lists.Forward (Iterator);
      end loop;
   end Pollute_Node;

   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d) is

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Position : Region_Position;
      Region   : Region_Id;
   begin
      Pool := Create_Position_Pool_From_Area (Manager, Area);
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Position := Get_Current (Iterator);
         Next (Iterator);
         Region := Get_Region_If_Exists (Manager, Position);
         if Region /= null then
            Add_Background_Pollution (Region);
         end if;
      end loop;
   end Pollute_Area;

   procedure Start_Refresh_Background
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area    :    out Rectangle_2d_Lists.List;
      Refresh_Pending : in     Boolean) is

      Pool     : Position_Pool;
      Iterator : Position_Iterator;
      Position : Region_Position;
      Region   : Region_Id;
   begin
      Refresh_Area := Rectangle_2d_Lists.Create;
      Pool := Create_Position_Pool_From_Area (Manager, Display_Area);
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Position := Get_Current (Iterator);
         Next (Iterator);
         Region := Get_Region_If_Exists (Manager, Position);
         if Region /= null and then Is_Background_Polluted (Region) then
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Region),
               Refresh_Area);
            if Refresh_Pending then
               Remove_Background_Pollution (Region);
            end if;
         else
            --  'Positions' without 'Region_Id's will be refreshed by default
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Manager, Position),
               Refresh_Area);
         end if;
      end loop;
   end Start_Refresh_Background;

   procedure End_Refresh_Background
     (Refresh_Area : in out Rectangle_2d_Lists.List) is
   begin
      Rectangle_2d_Lists.Destroy (Refresh_Area);
   end End_Refresh_Background;

   procedure Start_Refresh_Foreground
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area    :    out Rectangle_2d_Lists.List;
      Edges           :    out Edge_Update_Iterators.Merger_Access;
      Nodes           :    out Node_Update_Iterators.Merger_Access;
      Refresh_Pending : in     Boolean) is

      package Edge_Iterator_Lists renames
        Edge_Update_Iterators.Iterator_Lists;
      package Node_Iterator_Lists renames
        Node_Update_Iterators.Iterator_Lists;
      Pool           : Position_Pool;
      Iterator       : Position_Iterator;
      Position       : Region_Position;
      Region         : Region_Id;
      Edge_Iterators : Edge_Iterator_Lists.List;
      Node_Iterators : Node_Iterator_Lists.List;
   begin
      Refresh_Area := Rectangle_2d_Lists.Create;
      Edge_Iterators := Edge_Iterator_Lists.Create;
      Node_Iterators := Node_Iterator_Lists.Create;

      Pool := Create_Position_Pool_From_Area (Manager, Display_Area);
      Make_Position_Iterator (Pool, Iterator);
      while Has_More (Iterator) loop
         Position := Get_Current (Iterator);
         Next (Iterator);
         Region := Get_Region_If_Exists (Manager, Position);
         if Region /= null then
            Rectangle_2d_Lists.Attach
              (Get_Region_Extent (Region),
               Refresh_Area);
            Edge_Iterator_Lists.Attach
              (Get_Polluted_Edges (Region),
               Edge_Iterators);
            Node_Iterator_Lists.Attach
              (Get_Polluted_Nodes (Region),
               Node_Iterators);
            if Refresh_Pending then
               Remove_Foreground_Pollution (Region);
            end if;
         end if;
      end loop;

      Edges := Edge_Update_Iterators.Create (Edge_Iterators);
      Nodes := Node_Update_Iterators.Create (Node_Iterators);
      Edge_Iterator_Lists.Destroy (Edge_Iterators);
      Node_Iterator_Lists.Destroy (Node_Iterators);
   end Start_Refresh_Foreground;

   procedure End_Refresh_Foreground
     (Refresh_Area : in out Rectangle_2d_Lists.List;
      Edges        : in out Edge_Update_Iterators.Merger_Access;
      Nodes        : in out Node_Update_Iterators.Merger_Access) is
   begin
      Rectangle_2d_Lists.Destroy (Refresh_Area);
      Edge_Update_Iterators.Destroy (Edges);
      Node_Update_Iterators.Destroy (Nodes);
   end End_Refresh_Foreground;

   procedure Initialize
     (Manager : in out Region_Manager) is
   begin
      Manager.Regions := Region_Mappings.Create;
   end Initialize;

   procedure Finalize
     (Manager : in out Region_Manager) is
   begin
      Region_Mappings.Destroy (Manager.Regions);
   end Finalize;

   procedure Adjust
     (Manager : in out Region_Manager) is
   begin
      raise Region_Manager_Assignment_Unimplemented;
   end Adjust;


   -----------------------
   -- Region_Position,  --
   -- Position_Pool,    --
   -- Position_Iterator --
   -----------------------

   function Create_Position_Pool_From_Area
     (Manager      : in     Region_Manager;
      Area         : in     Vis.Absolute.Rectangle_2d)
     return Position_Pool is
   begin
      return Create_Position_Pool
        (Top_Left     => Get_Region_Position
                           (Manager, Vis.Absolute.Get_Top_Left (Area)),
         Bottom_Right => Get_Region_Position
                           (Manager, Vis.Absolute.Get_Bottom_Right (Area)));
   end Create_Position_Pool_From_Area;

   function Create_Position_Pool
     (Top_Left     : in     Region_Position;
      Bottom_Right : in     Region_Position)
     return Position_Pool is
   begin
      return Position_Pool (Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Vector_2d (Top_Left),
         Bottom_Right => Vis.Absolute.Vector_2d (Bottom_Right)));
   end Create_Position_Pool;

   function Get_Position_Pool_Area
     (Manager      : in     Region_Manager;
      Pool         : in     Position_Pool)
     return Vis.Absolute.Rectangle_2d is

      Top_Left_Position : Region_Position     := Region_Position
        (Vis.Absolute.Get_Top_Left (Vis.Absolute.Rectangle_2d (Pool)));
      Bottom_Right_Position : Region_Position := Region_Position
        (Vis.Absolute.Get_Bottom_Right (Vis.Absolute.Rectangle_2d (Pool)));
   begin
      return Vis.Absolute.Combine_Rectangle
        (Top_Left     => Vis.Absolute.Get_Top_Left
           (Get_Region_Extent (Manager, Top_Left_Position)),
         Bottom_Right => Vis.Absolute.Get_Bottom_Right
           (Get_Region_Extent (Manager, Bottom_Right_Position)));
   end Get_Position_Pool_Area;

   procedure Make_Position_Iterator
     (Pool     : in     Position_Pool;
      Iterator :    out Position_Iterator) is
   begin
      Iterator :=
        (Current => Region_Position
          (Vis.Absolute.Get_Top_Left (Vis.Absolute.Rectangle_2d (Pool))),
         Pool    => Pool);
   end Make_Position_Iterator;

   function Has_More
     (Iterator : in     Position_Iterator)
     return Boolean is
   begin
      return Vis.Absolute.Get_X (Vis.Absolute.Vector_2d (Iterator.Current))
        <= Vis.Absolute.Get_Right (Vis.Absolute.Rectangle_2d (Iterator.Pool));
   end Has_More;

   function Get_Current
     (Iterator : in     Position_Iterator)
     return Region_Position is
   begin
      pragma Assert (Has_More (Iterator));
      return Iterator.Current;
   end Get_Current;

   procedure Next
     (Iterator : in out Position_Iterator) is

      Current_X : Vis.Absolute_Int := Vis.Absolute.Get_X
        (Vis.Absolute.Vector_2d (Iterator.Current));
      Current_Y : Vis.Absolute_Int := Vis.Absolute.Get_Y
        (Vis.Absolute.Vector_2d (Iterator.Current));
   begin
      if Current_X < Vis.Absolute.Get_Right
           (Vis.Absolute.Rectangle_2d (Iterator.Pool)) or else
         Current_Y >= Vis.Absolute.Get_Bottom
           (Vis.Absolute.Rectangle_2d (Iterator.Pool))
      then

         Vis.Absolute.Set_X
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            X      => Current_X + 1);
      else
         Vis.Absolute.Set_X
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            X      => Vis.Absolute.Get_Left
                        (Vis.Absolute.Rectangle_2d (Iterator.Pool)));
         Vis.Absolute.Set_Y
           (Vector => Vis.Absolute.Vector_2d (Iterator.Current),
            Y      => Current_Y + 1);
      end if;
   end Next;

end Giant.Vis_Data;
