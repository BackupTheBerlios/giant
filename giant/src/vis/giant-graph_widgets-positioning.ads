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
--  $RCSfile: giant-graph_widgets-positioning.ads,v $, $Revision: 1.8 $
--  $Author: keulsn $
--  $Date: 2003/09/22 01:40:13 $
--
------------------------------------------------------------------------------
--
--  This package provides subprograms to calculate the absolute
--  positions of edges and nodes inside a graph widget. The following
--  process should be performed:
--  1. The logical positions of nodes (only) are set by a layouter
--     (see Giant.Layout_Factory)
--  2. A zoom level is chosen
--  3. The size of each node and each edge is calculated according to
--     the current detail level and zoom level. Because this depends
--     strongly on the way drawing is done subprograms for this
--     step are found in 'Giant.Graph_Widgets.Drawing'
--  4. This package is used to calculate the absolute positions of
--     edges and nodes
--  5. The edges and nodes are inserted into a region manager
--  6. The graph widget can be drawn
--
--  The positioning process takes 5 steps:
--  4.1. Setting the zoom level
--       use 'Set_Zoom'
--  4.2. Updating the position of each node according to zoom level
--       use 'Update_Node_Position'
--  4.3. Aligning the edges between their source and target nodes
--       use 'Update_Edge_Position'
--  4.4. Repositioning of edges that dock onto the same port in one node.
--       A node is surrounded by an infinite number of ports. An edge is
--       said to dock onto such a port, if the edge hits that port in
--       a straight line.
--       use 'Adjust_Ports'
--  4.5. Calculating the arrow-head for each edge
--       use 'Adjust_Arrow'
--
--  If the layout does not need to be optimal, then step 4.4 can be omitted,
--  however all other steps should be executed.
--


with Ada.Numerics;

package Giant.Graph_Widgets.Positioning is

   ----------------
   -- Life cycle --
   ----------------

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level);

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class);

   procedure Set_Zoom
     (Widget : access Graph_Widget_Record'Class;
      Zoom   : in     Vis.Zoom_Level);

   function Get_Zoom
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level;


   ---------------------
   -- Transformations --
   ---------------------

   function Get_Absolute
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Logic.Vector_2d)
     return Vis.Absolute.Vector_2d;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Size   : in     Vis.Absolute_Int)
     return Vis.Logic_Float;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d)
     return Vis.Logic.Vector_2d;

   function Get_Logic
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d)
     return Vis.Logic.Rectangle_2d;


   -----------------
   -- Positioning --
   -----------------

   ----------------------------------------------------------------------------
   --  Moves the node to its correct position maintaining its size
   procedure Update_Node_Position
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Alignes 'Edge' between its source and target nodes
   procedure Update_Edge_Position
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Shifts edges aside that connect to the same port in 'Node'
   procedure Adjust_Ports
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Calculates the positions of the lines for the arrow head of 'Edge'
   procedure Adjust_Arrow
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id);

private

   Default_Dock_Spacing         : constant := 5;
   Default_Displacement_Spacing : constant := 5;
   Default_Displacement_Angle   : constant Float := 0.2;
   Default_Loop_Radius          : constant := 20.0;
   Default_Loop_Separation      : constant := 2.0;

   Default_Edge_Label_Distance  : constant := 50.0;

   Default_Edge_Arrow_Angle     : constant Float :=
     (30.0 / 180.0) * Ada.Numerics.Pi;
   Default_Edge_Arrow_Length    : constant := 8.0;

end Giant.Graph_Widgets.Positioning;
