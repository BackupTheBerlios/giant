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
--  $RCSfile: giant-vis_data.ads,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/10 13:10:42 $
--
------------------------------------------------------------------------------
--
--  Provides data structures to handle the visual representation of edges
--  and nodes.
--


with Ada.Finalization;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Ordered_Sets;
with Lists;
pragma Elaborate_All (Lists);

with Giant.Graph_Lib;
with Giant.Vis;

package Giant.Vis_Data is


   ------------
   -- Layers --
   ------------

   ----------------------------------------------------------------------------
   --  Represents the layer in that an edge or a node lies
   --  In order to create new layers a Layer_Pool should be used.
   type Layer_Type is private;

   ----------------------------------------------------------------------------
   --  Tests if 'High' is a layer situated higher than 'Low'
   --
   --  Parameters:
   --    High - The first layer
   --    Low  - The second layer
   --  Returns:
   --    True if 'High' is higher than 'Low', False otherwise.
   function Is_Above
     (High : in     Layer_Type;
      Low  : in     Layer_Type)
     return Boolean;


   -----------------
   -- Layer Pools --
   -----------------

   ----------------------------------------------------------------------------
   --  Represents a pool of used layers. Must be reset using 'Reset_Pool'
   --  before a variable can be used.
   type Layer_Pool is private;

   ----------------------------------------------------------------------------
   --  Resets 'Pool' so it contains only the bottom layer
   procedure Reset_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Gets the highest layer in 'Pool'
   function Get_Highest
     (Pool : in     Layer_Pool)
     return Layer_Type;

   ----------------------------------------------------------------------------
   --  Adds a new layer to 'Pool' that will be top layer in 'Pool'
   procedure Enlarge_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Removes the top layer from 'Pool'
   procedure Shrink_Pool
     (Pool : in out Layer_Pool);


   -----------
   -- Edges --
   -----------

   ----------------------------------------------------------------------------
   --  Number of inflexion/begin/end points in one edge
   subtype Edge_Point_Number is Positive range 2 .. Positive'Last;

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of an edge
   type Vis_Edge_Record (Number_Of_Points : Edge_Point_Number) is
     limited private;

   type Vis_Edge_Id is access all Vis_Edge_Record;

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Edge_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Edge_Above
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Edge_Id's
   package Vis_Edge_Sets is new Ordered_Sets
     (Item_Type  => Vis_Edge_Id,
      "<"        => Is_Edge_Above);

   type Vis_Edge_Id_Array is array (Positive range <>) of Vis_Edge_Id;
   type Vis_Edge_Id_Array_Access is access Vis_Edge_Id_Array;


   -----------
   -- Nodes --
   -----------

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of a node
   type Vis_Node_Record is limited private;

   type Vis_Node_Id is access all Vis_Node_Record;

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Node_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Node_Above
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Node_Id's
   package Vis_Node_Sets is new Ordered_Sets
     (Item_Type  => Vis_Node_Id,
      "<"        => Is_Node_Above);

   type Vis_Node_Id_Array is array (Positive range <>) of Vis_Node_Id;
   type Vis_Node_Id_Array_Access is access Vis_Node_Id_Array;


   --------------------
   -- Region Manager --
   --------------------

   ----------------------------------------------------------------------------
   --  Type used to manage a graph within a two dimensional embedding
   --  provides functionality to add edges and nodes, to remove them and
   --  to find intersection among them.
   type Region_Manager is new Ada.Finalization.Controlled with private;

   package Rectangle_2d_Lists is new Lists
     (ItemType => Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Initializes a managed graph display region. If at all then must be
   --  called before any other subprogram operating on 'Region_Manager'.
   --
   --  Gives the region manager an initial size and an initial number of
   --  managed nodes. This is only an estimate and the actual size can grow
   --  infinitely (except for storage limitations).
   --
   --  If this subprogram is not called then default values will be used.
   --
   --  Note:
   --    There is no 'Done_Region_Manager'. Any necessary finalization will
   --    be handled implicitely since 'Region_Manager' is a descendant of
   --    'Ada.Finalization.Limited_Controlled'.
   --  Parameters:
   --    Manager         - The object to initialize
   --    Size            - Estimate of the size of the display region
   --    Number_Of_Nodes - Estimate of the number of nodes managed
   procedure Init_Region_Manager
     (Manager         :    out Region_Manager;
      Size            : in     Vis.Absolute.Vector_2d;
      Number_Of_Nodes : in     Natural);

   ----------------------------------------------------------------------------
   --  Optimizes the size of a drawing area so that update operations in
   --  that area will be necessary as less often as possible.
   --
   --  Parameters:
   --    Manager - The region manager to optimize for
   --    Area    - The drawing area
   --  Postcondition:
   --    All points previously contained in 'Area' will still be contained in
   --    'Area'
   procedure Optimize_Drawing_Area
     (Manager : in     Region_Manager;
      Area    : in out Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Inserts one edge into the set of managed edges. All content above the
   --  visual representation of that edge and the visual representation itself
   --  are polluted.
   procedure Insert_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Drops one edge. This edge is not managed anymore. All content
   --  intersecting the visual representation of that edge are polluted.
   procedure Drop_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Inserts one node into the set of managed nodes. All content above the
   --  visual representation of that node and the visual representation
   --  itself are polluted.
   procedure Insert_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Drops one node. This node is not managed anymore. All content
   --  intersecting the visual representation of that node are polluted.
   procedure Drop_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Pollutes all content above the visual representation of one edge
   --  and the visual representation itself.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Edge    - The edge to be polluted
   procedure Pollute_Edge
     (Manager : in out Region_Manager;
      Edge    : in     Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Pollutes all content above the visual representation of one node and
   --  the visual representation itself.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Node    - The node to be polluted
   procedure Pollute_Node
     (Manager : in out Region_Manager;
      Node    : in     Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Pollutes all content and background within an area.
   --
   --  Parameters:
   --    Manager - The region manager
   --    Area    - Area to be polluted
   procedure Pollute_Area
     (Manager : in out Region_Manager;
      Area    : in     Vis.Absolute.Rectangle_2d);


   ----------------------------------------------------------------------------
   --  Determines how to refresh the visual representation. Receives as input
   --  an area that will be displayed. If the background within that
   --  area has been polluted then it needs to be redrawn.
   --  This procedure produces as output the area that should be refreshed.
   --  This might be smaller than or equal to the input area.
   --
   --  Parameters:
   --    Manager      - The region manager
   --    Display_Area - The area that will be drawn
   --    Refresh_Area - The part of 'Display_Area' that needs to be refreshed
   --    Refresh_Done - If set to True, then the pollution on the background
   --                   of 'Display_Area' is cleaned.
   --  Notes:
   --    * The Area 'Display_Area' minus 'Refresh_Area' can be non-empty.
   --    * No point outside of 'Refresh_Area' must be modified.
   procedure Start_Refresh_Background
     (Manager      : in out Region_Manager;
      Display_Area : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area :    out Rectangle_2d_Lists.List;
      Refresh_Done : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Deallocates the storage uses by 'Start_Refresh_Background'. This
   --  procedure should be called to deallocate storage after a refresh
   --  background operation has finished.
   --
   --  Parameters:
   --    Refresh_Area - Variable obtained by 'Start_Refresh_Background'
   --  Precondition:
   --    'Refresh_Area' was obtained through a call to
   --    'Start_Refresh_Background'
   --  Postcondition
   --    'Refresh_Area' is an invalid list.
   procedure End_Refresh_Background
     (Refresh_Area : in out Rectangle_2d_Lists.List);

   ----------------------------------------------------------------------------
   --  Determines how to refresh the visual representation. Receives as input
   --  an area that will be displayed. If the contents within that
   --  area have been polluted, then all edges and nodes within that are
   --  need to be refreshed.
   --  This procedure produces as output the area that should be refreshed.
   --  This might be smaller than or equal to the input area. Produces also
   --  an array of edges and an array of nodes to be drawn in that area.
   --
   --  Parameters:
   --    Manager      - The region manager
   --    Display_Area - The area that will be drawn
   --    Refresh_Area - The part of 'Display_Area' that needs to be refreshed
   --    Edges        - Access to an array of all edges that need to be drawn
   --    Nodes        - Access to an array of all nodes that need to be drawn
   --    Refresh_Done - If set to True, then all pollution is cleaned from
   --                   Refresh_Area, else 'Manager' remains unchanged.
   --  Notes:
   --    * The Area 'Display_Area' minus 'Refresh_Area' can be non-empty.
   --      In that case the old content must be remembered. If the old
   --      content is forgotten then 'Pollute_Area' must be called on
   --      'Display_Area' before the call to 'Start_Refresh_Foreground'
   --    * During the drawing no point outside of 'Refresh_Area' must be
   --      modified. GtkAda provides filters to ensure this.
   --    * Before drawing begins, the background must be refreshed according
   --      to the data provided by 'Start_Refresh_Background'
   --    * The arrays 'Edges' and 'Nodes' are not sorted. Before drawing
   --      begins they should be sorted with Layer ascending. Drawing should
   --      begin with the lowest edge and end with the highest node.
   --    * 'End_Refresh_Operation' must be called after the refresh operation
   --      is done to deallocate storage.
   procedure Start_Refresh_Foreground
     (Manager      : in out Region_Manager;
      Display_Area : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area :    out Rectangle_2d_Lists.List;
      Edges        :    out Vis_Edge_Id_Array_Access;
      Nodes        :    out Vis_Node_Id_Array_Access;
      Refresh_Done : in     Boolean := True);

   ----------------------------------------------------------------------------
   --  Deallocates the storage used by 'Start_Refresh_Foreground'. This
   --  procedure should be called to deallocate storage after a refresh
   --  operation has finished.
   --
   --  Parameters:
   --    Refresh_Area - Variable obtained by 'Start_Refresh_Foreground'
   --    Edges        - Variable obtained by 'Start_Refresh_Foreground'
   --    Nodes        - Variable obtained by 'Start_Refresh_Foreground'
   --  Precondition:
   --    'Refresh_Area', 'Edges' and 'Nodes' were obtained through a call
   --    to 'Start_Refresh_Foreground'
   --  Postcondition:
   --    'Refresh_Area' contains an invalid List,
   --    'Edges' = null, 'Nodes' = null
   procedure End_Refresh_Foreground
     (Refresh_Area : in out Rectangle_2d_Lists.List;
      Edges        : in out Vis_Edge_Id_Array_Access;
      Nodes        : in out Vis_Node_Id_Array_Access);

private

   ----------------------------------------------------------------------------
   --  Height level of a Vis_Edge or a Vis_Node. The higher the value, the
   --  higher the Vis_Edge/Node_Edge
   type Layer_Type is new Natural;

   ----------------------------------------------------------------------------
   --  Top layer in the pool.
   type Layer_Pool is new Layer_Type;

   ----------------------------------------------------------------------------
   --  Container that covers a certain region. Contains an estimate of all
   --  'Vis_Edge_Id's and all 'Vis_Node_Id's that have intersection with
   --  that region.
   type Region_Record;

   type Region_Id is access all Region_Record;

   ----------------------------------------------------------------------------
   --  Top-Left point of a region, also used for 'Hash_Region_Position'
   type Region_Position is new Vis.Absolute.Vector_2d;

   ----------------------------------------------------------------------------
   --  Hash function on 'Region_Position's. 'Region_Position' models the top
   --  left point of a region. Since regions are always disjunct, no two
   --  regions can have the same top left point.
   --
   --  Parameters:
   --    Key - Top left point of a region
   --  Returns:
   --    Hash value
   --  Postcondition:
   --    For all A, B: Region_Position:
   --      (A = B)  ==>  (Hash_Region_Position (A) = Hash_Region_Position (B))
   function Hash_Region_Position
     (Key : in Region_Position)
     return Integer;

   ----------------------------------------------------------------------------
   --  Linear lists of 'Region_Id's
   package Region_Lists is new Lists
     (ItemType => Region_Id);

   package Region_Mappings is new Hashed_Mappings
     (Key_Type   => Region_Position,
      Hash       => Hash_Region_Position,
      Value_Type => Region_Id);

   ----------------------------------------------------------------------------
   --  Array of Points
   type Absolute_Point_Array is array
     (Positive range <>)
     of Vis.Absolute.Vector_2d;

   type Vis_Edge_Record (Number_Of_Points : Edge_Point_Number) is
      record
         --  Represented edge
         Edge              : Graph_Lib.Edge_Id;
         --  Source node of this edge
         Source            : Vis_Node_Id;
         --  Target node of this edge
         Target            : Vis_Node_Id;
         --  Width of this edge
         Width             : Vis.Absolute_Int;
         --  Hight level of this edge
         Layer             : Layer_Type;
         --  Start point, inflection points, end point
         Points            : Absolute_Point_Array (1 .. Number_Of_Points);
         --  Left point of arrow
         Left_Arrow_Point  : Vis.Absolute.Vector_2d;
         --  Right point of arrow
         Right_Arrow_Point : Vis.Absolute.Vector_2d;
         --  (Over-)Estimate of regions this edge is contained in
         Regions           : Region_Lists.List;
      end record;

   type Vis_Node_Record is
      record
         --  Represented node
         Node           : Graph_Lib.Node_Id;
         --  Top middle point
         Position       : Vis.Logic.Vector_2d;
         --  Hight level of this node
         Layer          : Layer_Type;
         --  Rectangle of this node
         Extent         : Vis.Absolute.Rectangle_2d;
         --  Edges with this node as target
         Incoming_Edges : Vis_Edge_Sets.Set;
         --  Edges with this node as source
         Outgoing_Edges : Vis_Edge_Sets.Set;
         --  (Over-)Estimate of regions this node is contained in
         Regions        : Region_Lists.List;
      end record;

   type Region_Record is limited
      record
         --  Size of this region
         Extent           : Vis.Absolute.Rectangle_2d;
         --  All contents in this region above the level are polluted
         Pollution_Height : Layer_Type;
         --  Edges possibly contained in or intersecting this region
         Edges            : Vis_Edge_Sets.Set;
         --  Nodes possibly contained in or intersecting this region
         Nodes            : Vis_Node_Sets.Set;
      end record;

   type Region_Manager is new Ada.Finalization.Controlled with
      record
         Region_Width  : Vis.Absolute_Natural;
         Region_Height : Vis.Absolute_Natural;
         Regions       : Region_Mappings.Mapping;
      end record;

   ----------------------------------------------------------------------------
   --  Inherited from Ada.Finalization.Controlled
   --  Must not be called by user.
   procedure Initialize
     (Manager : in out Region_Manager);

   ----------------------------------------------------------------------------
   --  Inherited from Ada.Finalization.Controlled
   --  Must not be called by user.
   procedure Adjust
     (Manager : in out Region_Manager);

   ----------------------------------------------------------------------------
   --  Inherited from Ada.Finalization.Limited_Controlled
   --  Must not be calles by user.
   procedure Finalize
     (Manager : in out Region_Manager);

end Giant.Vis_Data;
