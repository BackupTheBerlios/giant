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
--  $RCSfile: giant-vis_data.adb,v $, $Revision: 1.3 $
--  $Author: keulsn $
--  $Date: 2003/06/16 15:34:09 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;

package body Giant.Vis_Data is

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

   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean is
   begin
      return Is_Below (Left.Layer, Right.Layer);
   end Is_Edge_Below;


   -----------
   -- Nodes --
   -----------

   function Is_Node_Below
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean is
   begin
      return Is_Below (Left.Layer, Right.Layer);
   end Is_Node_Below;


   -------------
   -- Regions --
   -------------

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
     (Region : access Region_Record;
      Area   : in     Vis.Absolute.Rectangle_2d) is
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

   procedure Remove_Pollution
     (Region : access Region_Record) is
   begin
      Region.Polluted_Node := null;
      Region.Polluted_Edge := null;
      Region.Background_Polluted := False;
   end Remove_Pollution;

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

   function Intersects_Edge_Region
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id)
     return Boolean is
   begin
      ----------------------------------------
      -- Here's some serious coding missing --
      ----------------------------------------
      return True;
   end Intersects_Edge_Region;

   function Intersects_Node_Region
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id)
     return Boolean is
   begin
      return Vis.Absolute.Intersects (Region.Extent, Node.Extent);
   end Intersects_Node_Region;


   ---------------------
   -- Region_Managers --
   ---------------------

   procedure Init_Region_Manager
     (Manager         :    out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural) is
   begin
      null;--------------------------------------------------------------------
   end Init_Region_Manager;

   function Get_Region_Position
     (Manager : in     Region_Manager;
      Point   : in     Vis.Absolute.Vector_2d)
     return Region_Position is

      Position   : Region_Position;
      Point_X    : Vis.Absolute_Int := Vis.Absolute.Get_X (Point);
      Point_Y    : Vis.Absolute_Int := Vis.Absolute.Get_Y (Point);
      Position_X : Vis.Absolute_Int;
      Position_Y : Vis.Absolute_Int;
   begin
      Position_X := Point_X / Manager.Region_Width;
      Position_Y := Point_Y / Manager.Region_Height;
      if Point_X < 0 and then Point_X mod Manager.Region_Width /= 0 then
         Position_X := Position_X - 1;
      end if;
      if Point_Y < 0 and then Point_Y mod Manager.Region_Height /= 0 then
         Position_Y := Position_Y - 1;
      end if;
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

   --  Gets the region for a position. If that region does not exist yet then
   --  it will be created.
   function Get_Region
     (Manager  : in     Region_Manager;
      Position : in     Region_Position)
     return Region_Id is

      New_Region : Region_Id;
   begin
      if Region_Mappings.Is_Bound (Manager.Regions, Position) then
         return Region_Mappings.Fetch (Manager.Regions, Position);
      else
         New_Region := Create_Region (Get_Region_Extent (Position));
         Region_Mappings.Bind (Manager.Regions, Position, New_Region);
         return New_Region;
      end if;
   end Get_Region;

   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d) is
   begin
      null;--------------------------------------------------------------------
   end Optimize_Drawing_Area;

   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is
   begin
      null;--------------------------------------------------------------------
   end Insert_Edge;

   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is
   begin
      while not Region_Lists.IsEmpty (Edge.Regions) loop
         Remove_Edge_From_Region
           (Region_Lists.FirstValue (Edge.Regions), Edge);
         Region_Lists.DeleteHead
           (Edge.Regions);
      end loop;
   end Drop_Edge;

   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is
   begin
      null;--------------------------------------------------------------------
   end Insert_Node;

   procedure Drop_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is
   begin
      while not Region_Lists.IsEmpty (Node.Regions) loop
         Remove_Node_From_Region
           (Region_Lists.FirstValue (Node.Regions), Node);
         Region_Lists.RemoveHead
           (Node.Regions);
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
         Current_Region := Region_Lists.Cell (Iterator);
         Add_Edge_Pollution (Current_Region, Get_Layer (Edge));
         Region_Lists.Forward (Iterator);
      end loop;
      Region_Lists.Destroy (Iterator);
   end Pollute_Edge;

   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is

      Iterator       : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Node.Regions);
      Current_Region : Region_Id;
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.Cell (Iterator);
         Add_Node_Pollution (Current_Region, Get_Layer (Node));
         Region_Lists.Forward (Iterator);
      end loop;
      Region_Lists.Destroy (Iterator);
   end Pollute_Node;

   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d) is
   begin
      null;--------------------------------------------------------------------
   end Pollute_Area;

   procedure Start_Refresh_Background
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area    :    out Rectangle_2d_Lists.List;
      Refresh_Pending : in     Boolean                   := True) is
   begin
      null;--------------------------------------------------------------------
   end Start_Refresh_Background;

   procedure End_Refresh_Background
     (Refresh_Area : in out Rectangle_2d_Lists.List) is
   begin
      Rectangle_2d_Lists.Destroy (Refresh_Area);
   end End_Refresh_Background;

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

end Giant.Vis_Data;
