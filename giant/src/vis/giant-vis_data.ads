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
--  $RCSfile: giant-vis_data.ads,v $, $Revision: 1.20 $
--  $Author: keulsn $
--  $Date: 2003/07/10 16:05:51 $
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
--           Action : Layer_Clipping_Change_Type;
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
     (Hidden, Annotated,
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

   function Create_Edge
     (Graph_Edge  : in     Graph_Lib.Edge_Id;
      Source      : in     Vis_Node_Id;
      Target      : in     Vis_Node_Id;
      Layer       : in     Layer_Type;
      Inflections : in     Natural := 0)
     return Vis_Edge_Id;

   --  Note: Must be manually
   --    * Removed from region manager
   --    * Removed from incident nodes
   procedure Destroy
     (Edge        : in out Vis_Edge_Id);

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

   function Is_Loop
     (Edge : in     Vis_Edge_Id)
     return Boolean;

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

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcontition:
   --    Get_Thickness (Edge) = Thickness
   procedure Set_Thickness
     (Edge      : in     Vis_Edge_Id;
      Thickness : in     Vis.Absolute_Natural);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Has_Text_Area (Edge) and Get_Size (Get_Text_Area (Edge))) = Size
   procedure Set_Text_Area_Size
     (Edge      : in     Vis_Edge_Id;
      Size      : in     Vis.Absolute.Vector_2d);

   --  Sets the position of the text area
   --
   --  Parameters:
   --    Edge       - This edge's text area will be moved
   --    Position   - Position, the text area will be moved to
   --    Align_Left - If True, then the text area will be left aligned relative
   --                 to 'Position' otherwise it will be right aligned relative
   --                 to 'Position'
   --    Align_Top  - If True, then the text area will be top aligned relative
   --                 to 'Position', otherwise it will be bottom aligned
   --                 relative to 'Position'
   --  Precondition:
   --    * The text area must not be contained in any region manager
   --    * The size of the text area must have been set using
   --      'Set_Text_Area_Size'
   procedure Move_Text_Area_To
     (Edge       : in     Vis_Edge_Id;
      Position   : in     Vis.Absolute.Vector_2d;
      Align_Left : in     Boolean;
      Align_Top  : in     Boolean);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    not Has_Text_Area (Edge)
   procedure Remove_Text_Area
     (Edge      : in     Vis_Edge_Id);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Point (Edge, Num) = Point
   procedure Set_Point
     (Edge      : in     Vis_Edge_Id;
      Num       : in     Positive;
      Point     : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Left_Arrow_Point (Edge) = Point
   procedure Set_Left_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Edge)
   --  Postcondition:
   --    Get_Right_Arrow_Point (Edge) = Point
   procedure Set_Right_Arrow_Point
     (Edge      : in     Vis_Edge_Id;
      Point     : in     Vis.Absolute.Vector_2d);

   ----------------------------------------------------------------------------
   --  Total ordering on Vis_Edge_Id
   --  Returns:
   --    True if 'Left' is situated in a layer above 'Right', False else
   function Is_Edge_Below
     (Left  : in     Vis_Edge_Id;
      Right : in     Vis_Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Linear list of 'Vis_Edge_Id's
   package Vis_Edge_Lists is new Lists
     (ItemType   => Vis_Edge_Id);

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

   function Create_Node
     (Graph_Node : in     Graph_Lib.Node_Id;
      Layer      : in     Layer_Type)
     return Vis_Node_Id;

   --  Note: Must be manually
   --    * Removed from region manager
   --    * Removed from incident edges
   procedure Destroy
     (Node       : in out Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  Gets the node for the visual representation
   function Get_Graph_Node
     (Node : in     Vis_Node_Id)
     return Graph_Lib.Node_Id;

   function Get_Position
     (Node : in     Vis_Node_Id)
     return Vis.Logic.Vector_2d;

   function Get_Top_Center
     (Node : in     Vis_Node_Id)
     return Vis.Absolute.Vector_2d;

   function Get_Center
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

   procedure Make_Incoming_Iterator
     (Node           : in     Vis_Node_Id;
      Incoming_Edges :    out Vis_Edge_Lists.ListIter);

   procedure Make_Outgoing_Iterator
     (Node           : in     Vis_Node_Id;
      Outgoing_Edges :    out Vis_Edge_Lists.ListIter);

   function Is_Hidden
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Is_Annotated
     (Node : in     Vis_Node_Id)
     return Boolean;

   function Get_Highlighting
     (Node : in     Vis_Node_Id)
     return Flags_Type;

   procedure Set_Position
     (Node     : in Vis_Node_Id;
      Position : in Vis.Logic.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Node)
   procedure Set_Node_Size
     (Node : in     Vis_Node_Id;
      Size : in     Vis.Absolute.Vector_2d);

   --  Precondition:
   --    for all Rm : Region_Manager : not Contains (Rm, Node)
   procedure Move_Node
     (Node   : in     Vis_Node_Id;
      Offset : in     Vis.Absolute.Vector_2d);

   procedure Set_Annotated
     (Node   : in     Vis_Node_Id;
      State  : in     Boolean);

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
   --  The Assignment is nevertheless forbidden and can lead to undefined
   --  behavior.
   type Region_Manager is private;

   package Rectangle_2d_Lists is new Lists
     (ItemType => Vis.Absolute.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Must be called before the region manager can be used.
   --
   --  Parameters:
   --    Manager - The object to initialize
   procedure Set_Up
     (Manager : in out Region_Manager);

   ----------------------------------------------------------------------------
   --  Frees all storage used by a region manager. Leaves the region manager
   --  in an undefined state.
   --
   --  Parameters:
   --    Manager - The object to destroy. Must not be used anymore
   procedure Destroy
     (Manager : in out Region_Manager);

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
   --  Returns True if 'Edge' is contained in a region manager,
   --  False otherwise.
   --  Note:
   --    An edge can be contained in one region manager at most.
   function Has_Manager
     (Edge    : in     Vis_Edge_Id)
     return Boolean;

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
   --  Returns True if 'Node' is contained in a region manager,
   --  False otherwise.
   --  Note:
   --    An edge can be contained in one region manager at most.
   function Has_Manager
     (Node    : in     Vis_Node_Id)
     return Boolean;

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
   --  Command used to organize a buffer refresh for the content af a region
   --  manager. Refresh must be done in the order:
   --  1. Background
   --  2. Edges
   --  3. Nodes
   --  (4. Unchanged space)
   --
   --  Fields:
   --    Reset         - The rectangles are areas to be reset to background
   --                    color. If a rectangle is drawn, then clipping on
   --                    that rectangle is opened for edge and node drawing.
   --    Edge_Clipping - Clipping of a specified rectangle opens before any
   --                    edge in the same layer is drawn.
   --    Edges         - All edges to be drawn in correct (bottom-to-top)
   --                    order. Only drawing inside open clipping area is
   --                    needed. Other drawing must be avoided or removed
   --                    later.
   --    Node_Clipping - Same as Edge_Clipping, but for nodes. Note that
   --                    all rectangles opened by Reset or by Edge_Clipping
   --                    are open as well
   --    Nodes         - Same as Edges for nodes
   --    Unchanged     - All rectangles where clipping remained closed. The
   --                    previous content should remain here unchanged.
   --  Note:
   --    * NO rectangle may be contained in more than one "list" of
   --      Reset, Edge_Clipping, Node_Clipping, Unchanged
   --    * The desired area must be covered entirely by the rectangles in
   --      Reset, Edge_Clipping, Node_Clipping, Unchanged
   type Refresh_Command_Type is
      record
         Reset          : Rectangle_2d_Lists.List;
         Edge_Clipping  : Clipping_Queue_Access;
         Edges          : Edge_Update_Iterators.Merger_Access;
         Node_Clipping  : Clipping_Queue_Access;
         Nodes          : Node_Update_Iterators.Merger_Access;
         Unchanged      : Rectangle_2d_Lists.List;
      end record;

   procedure Start_Refresh_Operation
     (Manager         : in out Region_Manager;
      Refresh_Area    : in     Vis.Absolute.Rectangle_2d;
      Command         :    out Refresh_Command_Type;
      Refresh_Pending : in     Boolean);

   procedure End_Refresh_Operation
     (Command         : in out Refresh_Command_Type);

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
         --  Position of this node in logical space
         Position       : Vis.Logic.Vector_2d;
         --  Represented node
         Node           : Graph_Lib.Node_Id;
         --  Rectangle of this node
         Extent         : Vis.Absolute.Rectangle_2d;
         --  Hight level of this node
         Layer          : Layer_Type;
         --  Edges with this node as target
         Incoming_Edges : Vis_Edge_Lists.List;
         --  Edges with this node as source
         Outgoing_Edges : Vis_Edge_Lists.List;
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


   Default_Region_Width  : constant := 200;
   Default_Region_Height : constant := 300;

   type Region_Manager is new Ada.Finalization.Controlled with
      record
         Region_Width              : Vis.Absolute_Natural;
         Region_Height             : Vis.Absolute_Natural;
         Regions                   : Region_Mappings.Mapping;
         --  If True then background space not covered by regions must
         --  be refreshed.
         Empty_Background_Polluted : Boolean;
      end record;

end Giant.Vis_Data;
