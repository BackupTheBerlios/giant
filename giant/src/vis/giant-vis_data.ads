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
--  $RCSfile: giant-vis_data.ads,v $, $Revision: 1.14 $
--  $Author: keulsn $
--  $Date: 2003/06/30 14:37:49 $
--
------------------------------------------------------------------------------
--
--  Provides data structures to handle the visual representation of edges
--  and nodes.
--


with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Ordered_Sets;
with Lists;
pragma Elaborate_All (Lists);

with Giant.Graph_Lib;
with Giant.Merging_Iterators;
with Giant.Simple_Priority_Queues;
with Giant.Vis;

package Giant.Vis_Data is


   --  To be removed when implementation is done.
   Unimplemented : exception;


   ------------
   -- Layers --
   ------------

   ----------------------------------------------------------------------------
   --  Represents the layer in that an edge or a node lies
   --  In order to create new layers a Layer_Pool should be used.
   type Layer_Type is private;

   ----------------------------------------------------------------------------
   --  Tests if 'Low' is a layer situated below the layer of 'High'
   --
   --  Parameters:
   --    Low  - The first layer
   --    High - The second layer
   --  Returns:
   --    True if 'Low' is below 'High', False otherwise.
   function Is_Below
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Tests if 'Low' is a layer situated below the layer of 'High' or
   --  if 'Low = High'
   --
   --  Parameters:
   --    Low  - The first layer
   --    High - The second layer
   --  Returns:
   --    True if 'Low' is below or at level with 'High', False otherwise.
   function Is_Below_Or_Equal
     (Low  : in     Layer_Type;
      High : in     Layer_Type)
     return Boolean;


   -----------------
   -- Layer Pools --
   -----------------

   ----------------------------------------------------------------------------
   --  Represents a pool of used layers. Must be reset using 'Reset_Pool'
   --  before a variable can be used.
   type Layer_Pool is private;

   ----------------------------------------------------------------------------
   --  The layer for which:
   --  for all L : Layer_Pool: Is_Below_Or_Equal (Bottom_Layer, L)
   Bottom_Layer : constant Layer_Pool;

   ----------------------------------------------------------------------------
   --  The layer for which:
   --  for all L : Layer_Pool: Is_Below_Or_Equal (L, Top_Layer)
   Top_Layer : constant Layer_Pool;

   ----------------------------------------------------------------------------
   --  Resets 'Pool' so it contains only 'Bottom_Layer'
   procedure Reset_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Gets the highest layer in 'Pool'
   function Get_Highest_Layer
     (Pool : in     Layer_Pool)
     return Layer_Type;

   ----------------------------------------------------------------------------
   --  Adds a new layer to 'Pool' that will be the highest layer in 'Pool'
   --  Precondition:
   --    Is_Below (Get_Highest_Layer, Top_Layer)
   --  Raises:
   --    Constraint_Error if Precondition not satisfied
   procedure Enlarge_Pool
     (Pool : in out Layer_Pool);

   ----------------------------------------------------------------------------
   --  Removes the highest layer from 'Pool'
   --  Precondition:
   --    Is_Below (Bottom_Layer, Get_Highest_Layer (Pool))
   --  Raises:
   --    Constraint_Error if Precondition not satisfied
   procedure Shrink_Pool
     (Pool : in out Layer_Pool);


   --------------
   -- Clipping --
   --------------

   type Layer_Clipping_Change_Type is (Add, Delete);

   type Layer_Clipping_Type is
      record
         Height : Layer_Type;
         Action : Layer_Clipping_Change_Type;
         Area   : Vis.Absolute.Rectangle_2d;
      end record;

   type Layer_Clipping_Access is access Layer_Clipping_Type;

   procedure Free
     (Clipping : in out Layer_Clipping_Access);

   function Is_Below
     (Left  : in     Layer_Clipping_Access;
      Right : in     Layer_Clipping_Access)
     return Boolean;

   package Clipping_Queues is new Simple_Priority_Queues
     (Item_Type           => Layer_Clipping_Access,
      Has_Higher_Priority => Is_Below);

   type Clipping_Queue_Access is access Clipping_Queues.Queue_Type;


   -----------
   -- Flags --
   -----------

   type Flags_Enumeration_Type is
     (Hidden,
      Current_Local, First_Local, Second_Local, Third_Local,
      First_Global, Second_Global, Third_Global);

   type Flags_Type is array (Flags_Enumeration_Type) of Boolean;
   pragma Pack (Flags_Type);

   subtype Highlight_Type is
     Flags_Enumeration_Type range Current_Local .. Third_Global;

   subtype Local_Highlight_Type is
     Highlight_Type range Current_Local .. Third_Local;

   subtype Global_Highlight_Type is
     Highlight_Type range First_Global .. Third_Global;


   --------------------
   -- Edges & Nodes  --
   --------------------

   ----------------------------------------------------------------------------
   --  Number of inflexion/begin/end points in one edge
   subtype Edge_Point_Number is Positive range 2 .. Positive'Last;

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of an edge
   type Vis_Edge_Record (Number_Of_Points : Edge_Point_Number) is
     limited private;

   type Vis_Edge_Id is access all Vis_Edge_Record;

   ----------------------------------------------------------------------------
   --  Type for handling the visual representation of a node
   type Vis_Node_Record is limited private;

   type Vis_Node_Id is access all Vis_Node_Record;



   -----------
   -- Edges --
   -----------

   ----------------------------------------------------------------------------
   --  Gets the edge for the visual representation
   function Get_Graph_Edge
     (Edge : in     Vis_Edge_Id)
     return Graph_Lib.Edge_Id;

   function Get_Source
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id;

   function Get_Target
     (Edge : in     Vis_Edge_Id)
     return Vis_Node_Id;

   ----------------------------------------------------------------------------
   --  Gets the layer 'Edge' is inside. No other edge must be in the same
   --  layer. The user must ensure this.
   function Get_Layer
     (Edge  : in     Vis_Edge_Id)
     return Layer_Type;

   function Get_Thickness
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute_Natural;

   ----------------------------------------------------------------------------
   --  Number of Points for the main line of edge (excluding the arrow)
   function Get_Number_Of_Points
     (Edge : in     Vis_Edge_Id)
     return Edge_Point_Number;

   ----------------------------------------------------------------------------
   --  Get a point of the main line (excluding the arrow). The main line
   --  is drawn along the points returned by this function in ascending
   --  order of 'Num'.
   --
   --  Precondition:
   --    1 <= Num <= Get_Number_Of_Points
   function Get_Point
     (Edge : in     Vis_Edge_Id;
      Num  : in     Positive)
     return Vis.Absolute.Vector_2d;

   function Get_Left_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Right_Arrow_Point
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Vector_2d;

   function Has_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   function Get_Text_Area
     (Edge : in     Vis_Edge_Id)
     return Vis.Absolute.Rectangle_2d;

   function Is_Hidden
     (Edge : in     Vis_Edge_Id)
     return Boolean;

   function Get_Highlighting
     (Edge : in     Vis_Edge_Id)
     return Flags_Type;

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Edge_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Edge_Id's
   package Vis_Edge_Sets is new Ordered_Sets
     (Item_Type  => Vis_Edge_Id,
      "<"        => Is_Edge_Below);

   ----------------------------------------------------------------------------
   --  Iterators over 'Vis_Edge_Id's in Layer-ascending order
   package Edge_Update_Iterators is new Merging_Iterators
     (Item_Type => Vis_Edge_Id,
      "<"       => Is_Edge_Below,
      Sets      => Vis_Edge_Sets);

   type Vis_Edge_Id_Array is array (Positive range <>) of Vis_Edge_Id;
   type Vis_Edge_Id_Array_Access is access Vis_Edge_Id_Array;


   -----------
   -- Nodes --
   -----------

   ----------------------------------------------------------------------------
   --  Gets the node for the visual representation
   function Get_Graph_Node
     (Node : in     Vis_Node_Id)
     return Graph_Lib.Node_Id;

   function Get_Top_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Extent
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Gets the layer 'Node' is inside. No other node must be in the same
   --  layer. The user must ensure this.
   function Get_Layer
     (Node : in     Vis_Node_Id)
     return Layer_Type;

   --  Parameters:
   --    Node
   --    Incoming_Edges - The iterator, must be destroyed using
   --                     'Vis_Edge_Sets.Destroy'
   procedure Make_Incoming_Iterator
     (Node           : in     Vis_Node_Id;
      Incoming_Edges :    out Vis_Edge_Sets.Iterator);

   --  Parameters:
   --    Outgoing_Edges - The iterator, must be destroyed using
   --                     'Vis_Edge_Sets.Destroy'
   procedure Make_Outgoing_Iterator
     (Node           : in     Vis_Node_Id;
      Outgoing_Edges :    out Vis_Edge_Sets.Iterator);

   function Is_Hidden
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Get_Highlighting
     (Node : in     Vis_Node_Id)
     return Flags_Type;

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Node_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Node_Below
     (Left  : in     Vis_Node_Id;
      Right : in     Vis_Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Set of 'Vis_Node_Id's
   package Vis_Node_Sets is new Ordered_Sets
     (Item_Type  => Vis_Node_Id,
      "<"        => Is_Node_Below);

   ----------------------------------------------------------------------------
   --  Iterators over 'Vis_Node_Id's in Layer-ascending order
   package Node_Update_Iterators is new Merging_Iterators
     (Item_Type  => Vis_Node_Id,
      "<"        => Is_Node_Below,
      Sets       => Vis_Node_Sets);

   type Vis_Node_Id_Array is array (Positive range <>) of Vis_Node_Id;
   type Vis_Node_Id_Array_Access is access Vis_Node_Id_Array;


   --------------------
   -- Region Manager --
   --------------------

   ----------------------------------------------------------------------------
   --  Type used to manage a graph within a two dimensional embedding
   --  provides functionality to add edges and nodes, to remove them and
   --  to find intersection among them.
   --  Should be limited type, but is not in order to allow objects of this
   --  type to be fields in gtk-widget-types (which are not limited).
   --  The Assignment is nevertheless forbidden and
   --  will raise 'Region_Manager_Assignment_Unimplemented'
   type Region_Manager is new Ada.Finalization.Controlled with private;

   ----------------------------------------------------------------------------
   --  Is raised in the Adjust subprogram for Region_Manager. Assignment
   --  of Region_Manager instances is not implemented.
   Region_Manager_Assignment_Unimplemented : exception;

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
   --    be handled implicitly since 'Region_Manager' is a descendant of
   --    'Ada.Finalization.Controlled'.
   --  Parameters:
   --    Manager         - The object to initialize
   --    Size            - Estimate of the size of the display region
   --    Number_Of_Nodes - Estimate of the number of nodes managed
   procedure Init_Region_Manager
     (Manager         : in out Region_Manager;
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
   --    Manager         - The region manager
   --    Display_Area    - The area that will be drawn
   --    Refresh_Area    - The part of 'Display_Area' that needs to be
   --                      refreshed
   --    Refresh_Pending - If set to True, then the pollution on the
   --                      background of 'Display_Area' is cleaned.
   --  Notes:
   --    * The Area 'Display_Area' minus 'Refresh_Area' can be non-empty.
   --    * No point outside of 'Refresh_Area' must be modified.
   procedure Start_Refresh_Background
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Refresh_Area    :    out Rectangle_2d_Lists.List;
      Refresh_Pending : in     Boolean);

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
   --  Determines which edges to refresh in the visual representation.
   --  Receives as input an area to be displayed. If the contents in that
   --  area have been polluted then produces as output an iterator
   --  over all edges to be redrawn. Also provides a set of clipping
   --  instructions to be respected during drawing.
   --
   --  The data structures returned by this subprogram must be destroyed
   --  by a call to 'End_Edge_Refresh'.
   --  If (and only if) Items are removed from 'Clipping' than those items
   --  must be destroyed by a call to 'Free'.
   --
   --  Parameters:
   --    Manager         - The region manager
   --    Display_Area    - The area that will be displayed to the user. This
   --                      subprogram can decide to have an area redrawn that
   --                      is actually greater than 'Display_Area'. See
   --                      'Clipping'
   --    Clipping        - Height-ordered queue of clipping instructions. Each
   --                      such instruction must be honored when drawing of
   --                      edges comes to its layer.
   --    Edges           - Edges to be drawn. Ordered from lower to higher
   --                      layers
   --    Refresh_Pending - If set to True then pollution will be removed
   --                      from 'Edges'
   procedure Start_Edge_Refresh
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Clipping        :    out Clipping_Queue_Access;
      Edges           :    out Edge_Update_Iterators.Merger_Access;
      Refresh_Pending : in     Boolean);

   ----------------------------------------------------------------------------
   --  Destroys data structures obtained by a call to 'Start_Edge_Refresh'
   --
   --  Parameters:
   --    Clipping - The out-parameter from 'Start_Edge_Refresh'
   --    Edges    - The out-parameter from 'Start_Edge_Refresh'
   procedure End_Edge_Refresh
     (Clipping        : in out Clipping_Queue_Access;
      Edges           : in out Edge_Update_Iterators.Merger_Access);

   ----------------------------------------------------------------------------
   --  Determines which nodes to refresh in the visual representation.
   --  Receives as input an area to be displayed. If the contents in that
   --  area have been polluted then produces as output an iterator
   --  over all nodes to be redrawn. Also provides a set of clipping
   --  instructions to be respected during drawing.
   --
   --  The data structures returned by this subprogram must be destroyed
   --  by a call to 'End_Node_Refresh'.
   --  If (and only if) Items are removed from 'Clipping' than those items
   --  must be destroyed by a call to 'Free'.
   --
   --  Parameters:
   --    Manager         - The region manager
   --    Display_Area    - The area that will be displayed to the user. This
   --                      subprogram can decide to have an area redrawn that
   --                      is actually greater than 'Display_Area'. See
   --                      'Clipping'
   --    Clipping        - Height-ordered queue of clipping instructions. Each
   --                      such instruction must be honored when drawing of
   --                      nodes comes to its layer.
   --    Nodes           - Nodes to be drawn. Ordered from lower to higher
   --                      layers
   --    Refresh_Pending - If set to True then pollution will be removed
   --                      from 'Nodes'
   procedure Start_Node_Refresh
     (Manager         : in out Region_Manager;
      Display_Area    : in     Vis.Absolute.Rectangle_2d;
      Clipping        :    out Clipping_Queue_Access;
      Nodes           :    out Node_Update_Iterators.Merger_Access;
      Refresh_Pending : in     Boolean);

   ----------------------------------------------------------------------------
   --  Destroys data structures obtained by a call to 'Start_Node_Refresh'
   --
   --  Parameters:
   --    Clipping - The out-parameter from 'Start_Node_Refresh'
   --    Nodes    - The out-parameter from 'Start_Node_Refresh'
   procedure End_Node_Refresh
     (Clipping        : in out Clipping_Queue_Access;
      Nodes           : in out Node_Update_Iterators.Merger_Access);


private

   ----------------------------------------------------------------------------
   --  Height level of a Vis_Edge or a Vis_Node. The higher the value, the
   --  higher the Vis_Edge/Node_Edge
   type Layer_Type is new Natural;

   ----------------------------------------------------------------------------
   --  Top layer in the pool.
   type Layer_Pool is new Natural; --  Layer_Type;

   Bottom_Layer : constant Layer_Pool := Layer_Pool'First;
   Top_Layer    : constant Layer_Pool := Layer_Pool'Last;


   ----------------------------------------------------------------------------
   --  Frees all items and frees clipping queue
   procedure Destroy_Clipping_Queue
     (Queue : in out Clipping_Queue_Access);


   ----------------------------------------------------------------------------
   --  Container that covers a certain region. Contains an estimate of all
   --  'Vis_Edge_Id's and all 'Vis_Node_Id's that have intersection with
   --  that region.
   type Region_Record;

   type Region_Id is access all Region_Record;

   ----------------------------------------------------------------------------
   --  Region coordinate system
   type Region_Position is new Vis.Absolute.Vector_2d;

   ----------------------------------------------------------------------------
   --  Hash function on 'Region_Position's.
   --
   --  Parameters:
   --    Key - position of a region
   --  Returns:
   --    Hash value
   --  Postcondition:
   --    For all A, B: Region_Position:
   --      (A = B)  ==>  (Hash_Region_Position (A) = Hash_Region_Position (B))
   function Hash_Region_Position
     (Key : in Region_Position)
     return Integer;

   ----------------------------------------------------------------------------
   --  Strict order on 'Region_Position's
   --
   --  Parameters:
   --    Left  - position of a region
   --    Right - position of a region
   function Order_Position
     (Left  : in    Region_Position;
      Right : in    Region_Position)
     return Boolean;

   package Position_Sets is new Ordered_Sets
     (Item_Type => Region_Position,
      "<"       => Order_Position);

   ----------------------------------------------------------------------------
   --  A 'Position_Pool' represents a rectangle of 'Region_Position's
   type Position_Pool is new Vis.Absolute.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Get the 'Position_Pool' so that the regions that correspond to
   --  positions within the pool contain 'Area'.
   function Create_Position_Pool_From_Area
     (Manager      : in     Region_Manager;
      Area         : in     Vis.Absolute.Rectangle_2d)
     return Position_Pool;

   function Create_Position_Pool
     (Top_Left     : in     Region_Position;
      Bottom_Right : in     Region_Position)
     return Position_Pool;

   --  Get the area actually covered by 'Pool'. This area might contain more
   --  points than the 'Area' given into 'Create_Position_Pool_From_Area'
   function Get_Position_Pool_Area
     (Manager      : in     Region_Manager;
      Pool         : in     Position_Pool)
     return Vis.Absolute.Rectangle_2d;

   function Get_Position_Pool_Size
     (Pool         : in     Position_Pool)
     return Natural;

   ----------------------------------------------------------------------------
   --  Allows iteration over all 'Region_Position's in a 'Position_Pool'
   type Position_Iterator is
      record
         Current : Region_Position;
         Pool    : Position_Pool;
      end record;

   procedure Make_Position_Iterator
     (Pool     : in     Position_Pool;
      Iterator :    out Position_Iterator);

   function Has_More
     (Iterator : in     Position_Iterator)
     return Boolean;

   function Get_Current
     (Iterator : in     Position_Iterator)
     return Region_Position;

   procedure Next
     (Iterator : in out Position_Iterator);

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
         Thickness         : Vis.Absolute_Natural;
         --  Area where text can be drawn or else Get_Height = 'Last
         Text_Area         : Vis.Absolute.Rectangle_2d;
         --  Layer of this edge
         Layer             : Layer_Type;
         --  Start point, inflection points, end point
         Points            : Absolute_Point_Array (1 .. Number_Of_Points);
         --  Left point of arrow
         Left_Arrow_Point  : Vis.Absolute.Vector_2d;
         --  Right point of arrow
         Right_Arrow_Point : Vis.Absolute.Vector_2d;
         --  (Over-)Estimate of regions this edge is contained in
         Regions           : Region_Lists.List;
         --  Visual attributes of this edge
         Flags             : Flags_Type;
      end record;

   type Vis_Node_Record is
      record
         --  Represented node
         Node           : Graph_Lib.Node_Id;
         --  Rectangle of this node
         Extent         : Vis.Absolute.Rectangle_2d;
         --  Hight level of this node
         Layer          : Layer_Type;
         --  Edges with this node as target
         Incoming_Edges : Vis_Edge_Sets.Set;
         --  Edges with this node as source
         Outgoing_Edges : Vis_Edge_Sets.Set;
         --  (Over-)Estimate of regions this node is contained in
         Regions        : Region_Lists.List;
         --  Visual attributes of this node
         Flags          : Flags_Type;
      end record;

   type Region_Record is limited
      record
         --  Size of this region
         Extent              : Vis.Absolute.Rectangle_2d;
         --  All contents in this region above this 'Vis_Edge_Id' and this
         --  'Vis_Edge_Id' are polluted, or null
         Polluted_Edge       : Vis_Edge_Id;
         --  All contents in this region above this 'Vis_Node_Id' and this
         --  'Node_Id' are polluted, or null
         Polluted_Node       : Vis_Node_Id;
         --  The Background of this region and all contents are polluted
         --  iff True.
         Background_Polluted : Boolean;
         --  Edges possibly contained in or intersecting this region
         Edges               : Vis_Edge_Sets.Set;
         --  Nodes possibly contained in or intersecting this region
         Nodes               : Vis_Node_Sets.Set;
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
