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
--  $RCSfile: giant-vis_data.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/12 17:01:26 $
--
------------------------------------------------------------------------------


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

   procedure Add_Background_Pollution
     (Region : access Region_Record;
      Area   : in     Vis.Absolute.Rectangle_2d) is
   begin
      Region.Pollution_On_Edges := True;
      Region.Pollution_Height := Bottom_Layer;
   end Add_Background_Pollution;

   procedure Add_Edge_Pollution
     (Region : access Region_Record;
      Edge   : in     Vis_Edge_Id) is
   begin
      if Region.Pollution_On_Edges then
         Region.Pollution_Height := Layer_Type'Min
           (Region.Pollution_Height, Get_Layer (Edge));
      else
         Region.Pollution_On_Edges := True;
         Region.Pollution_Height := Get_Layer (Edge);
      end if;
   end Add_Edge_Pollution;

   procedure Add_Node_Pollution
     (Region : access Region_Record;
      Node   : in     Vis_Node_Id) is
   begin
      if not Region.Pollution_On_Edges then
         Region.Pollution_Height := Layer_Type'Min
           (Region.Pollution_Height, Get_Layer (Node));
      end if;
   end Add_Node_Pollution;

   procedure Remove_Pollution
     (Region : access Region_Record) is
   begin
      Region.Pollution_Height := Top_Layer;
      Region.Pollution_On_Edges := False;
   end Remove_Pollution;


   ---------------------
   -- Region_Managers --
   ---------------------

   procedure Init_Region_Manager
     (Manager         :    out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural);

   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d) is
   begin
   end Optimize_Drawing_Area;

   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is
   begin
   end Insert_Edge;

   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id) is
   begin
      while not Region_Lists.IsEmpty (Edge.Regions) loop
         Remove_Edge_From_Region
           (Region_Lists.FirstValue (Edge.Regions), Edge);
         Region_Lists.RemoveHead
           (Edge.Regions);
      end loop;
   end Drop_Edge;

   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is
   begin
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

      Iterator : Region_Lists.ListIter := Region_Lists.MakeListIter
        (Edge.Regions);
   begin
      while Region_Lists.More (Iterator) loop
         Current_Region := Region_Lists.Cell (Iterator);
         Add_Edge_Pollution (Current_Region, Get_Layer (Edge));
         Region_Lists.Forward (Iterator);
      end loop;
   end Pollute_Edge;

   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id) is
   begin
   end Pollute_Node;

   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d) is
   begin
   end Pollute_Area;

   procedure Start_Refresh_Background
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area    :    out Rectangle_2d_Lists.List;
      Refresh_Pending : in     Boolean                   := True) is
   begin
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
