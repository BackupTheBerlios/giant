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
--  $RCSfile: giant-graph_widgets.ads,v $, $Revision: 1.10 $
--  $Author: keulsn $
--  $Date: 2003/06/26 20:20:24 $
--
------------------------------------------------------------------------------
--
--  This package contains the graph widget. The graph widget is used to
--  display the graph within a GtkAda Window.
--
--  The user can perform the following actions within a graph widget, these
--  actions are handled by the graph widget:
--  * Resizing the graph widget
--  * Move a Node via Drag'n'Drop
--
--  The user can perform the following actions within a graph widget, these
--  actions are recognized by the graph widget and the graph widget's clients
--  will be notified. See package Graph_Widgets.Notifications.
--  * Open a PopUp Menu on the background of the graph widget
--  * Open a PopUp Menu on one specific edge
--  * Open a PopUp Menu on one specific node
--  * select the command to clear the current selection
--  * Select one specific edge or one specific node and only this item
--  * Invert the selection state of one specific edge/node
--  * Open a box and select the command to select all edges and nodes that
--    are situated within this frame (and only those).
--  * Open a box and select the command to invert the selection status of
--    all edges and nodes that are situated within this frame.
--  * Open a box and select the command to add all edges and nodes that are
--    situated within this frame to the current selection.
--
--  The graph widget can be controlled by the program to:
--  * Insert edges or nodes into the graph widget
--  * Remove edges or nodes from the graph widget
--  * Set the color scheme to a certain visualization style
--  * Display some edges or nodes in a certain "global" highlight color
--  * Display some edges or nodes in a certain "local" highlight color
--


with Ada.Streams;

with Gdk.Color;
with Gdk.Cursor;
with Gdk.Pixmap;
with Gtk.Widget;
pragma Elaborate_All (Gtk.Widget);

with Bauhaus_IO;
with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

with Giant.Config;
with Giant.Config.Global_Data;
with Giant.Config.Vis_Styles;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Vis;
with Giant.Vis_Data;

package Giant.Graph_Widgets is


   --  To be removed when implementation is done.
   Unimplemented : exception;


   -------------------
   -- Graph Widgets --
   -------------------

   ----------------------------------------------------------------------------
   --  A widget to display a graph
   type Graph_Widget_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Graph_Widget is access all Graph_Widget_Record'Class;


   ------------------------
   -- General Exceptions --
   ------------------------

   ----------------------------------------------------------------------------
   --  Raised whenever an Edge_Id is provided as an argument, but this
   --  Edge_Id was not previously inserted into the graph widget
   Unknown_Edge_Id : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a Node_Id is provided as an argument, but this
   --  Node_Id was not previously inserted into the graph widget
   Unknown_Node_Id : exception;


   -------------------------------
   -- Construction, Destruction --
   -------------------------------

   ----------------------------------------------------------------------------
   --  Creates an emty graph widget.
   --
   --  Parameters:
   --    Widget - Access to a new graph widget
   --    Style  - The visualization style to be used for 'Widget'
   procedure Create
     (Widget :    out Graph_Widget;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access
                       := Config.Vis_Styles.Get_Default_Vis_Style);

   ----------------------------------------------------------------------------
   --  Creates a new Graph_Widget and sets its state to the state stored in
   --  'Stream'.
   --
   --  Parameters:
   --    Stream - The stream to read the state from
   --    Widget - Access to a new graph widget
   --  Raises
   --    ...
   procedure Read_Graph_Widget
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Widget :    out Graph_Widget);

   ----------------------------------------------------------------------------
   --  Outputs a graph widget to 'Stream'. It can then be read into memory
   --  again using 'Input'
   --
   --  Parameters:
   --    Stream - The stream to output the state to
   --    Widget - The graph widget to output
   --  Raises
   --    ...
   procedure Write_Graph_Widget
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Widget : access Graph_Widget_Record);


   -------------------
   -- Configuration --
   -------------------

   --  Here are the configuration subprograms available for graph widgets.
   --  Note that the greatest part of the configuration is obtained
   --  directly from the packages 'Giant.Config.*'.
   --  See 'Giant.Graph_Widgets.Settings' for more configuration.

   ----------------------------------------------------------------------------
   --  Sets the cursor used inside the graph widget during the time when
   --  the graph widget is not in action mode and is not waiting for a lock
   --  to be released.
   --
   --  If this subprogram is never called, then the cursor for
   --  'Gdk.Types.Arrow' will be used.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor
   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   ----------------------------------------------------------------------------
   --  Sets the cursor used inside the graph widget during the time when
   --  the graph widget is waiting for a lock to be released or when
   --  the graph widget is performing heavy calculations.
   --
   --  If this subprogram is never called, then the cursor for
   --  'Gdk.Types.Clock' will be used.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor
   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);


   --------------------------------------------
   -- Insertion, Deletion of Edges and Nodes --
   --------------------------------------------

   ----------------------------------------------------------------------------
   --  Describes a Lock that a Layouter must acquire before making changes
   --  to the layout.
   type Lock_Type is private;

   ----------------------------------------------------------------------------
   --  Checks if an edge is contained in a graph widget
   --
   --  Note:
   --    Even if an edge is contained in the widget, it might still be
   --    hidden and thus the user might be unaware of its presence.
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The edge to be searched for
   --  Returns:
   --    True if 'Edge' is contained in 'Widget', False else
   function Contains
     (Widget   : access Graph_Widget_Record'Class;
      Edge     : in     Graph_Lib.Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Checks if a node is contained in a graph widget
   --
   --  Note:
   --    Even if a node is contained in the widget, it might still be
   --    hidden and thus the user might be unaware of its presence.
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - Node to be searched for
   --  Returns:
   --    True if 'Node' is contained in 'Widget', False else
   function Contains
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Inserts edges and nodes from 'Selection' into a 'Widget'. An edge
   --  is inserted only if its two incident nodes are contained in 'Widget'.
   --
   --  This subprogram produces a lock for the inserted selection. The
   --  inserted selection is locked until 'Lock' is release by a call
   --  to 'Release_Lock' (see below). Until then the newly inserted selection
   --  is not shown inside the graph widget.
   --
   --  Usually a layouter should be started on the selection and after that
   --  layouter has finished the lock should be released.
   --
   --  Note:
   --    The user probably cannot perform action on the graph widget until
   --    the lock is released.
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - A collection of edges and nodes
   --    Lock      - A lock for the inserted selection
   --  Postcondition:
   --    For all N: Node_Id in 'Selection': 'Contains (Widget, N)'
   --    For all E: Edge_Id in 'Selection': 'Contains (Widget, E)' if and only
   --      if {source (E), target (E)} is a subset of
   --      'Selection' union {N: Node_Id | 'Contains (Widget, N)'}
   procedure Insert_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Removes all edges and all nodes from 'Selection' that 'Widget' contains
   --  already. Then, it inserts the remaining edges and nodes into 'Widget'.
   --
   --  This subprogram produces a lock for the inserted selection. The
   --  inserted selection is locked until 'Lock' is release by a call
   --  to 'Release_Lock' (see below). Until then the newly inserted selection
   --  is not shown inside the graph widget.
   --
   --  Usually a layouter should be started on the selection and after that
   --  layouter has finished the lock should be released.
   --
   --  Note:
   --    * The contents of 'Selection' are modified by this subprogram!
   --    * The user probably cannot perform actions on the graph widget until
   --      the lock is released.
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Set of edges and nodes that should be inserted into
   --                'Widget', also used as out-Parameter
   --  Postcondition:
   --    * 'Widget' contains all edges and nodes that it contained before the
   --      call.
   --    * 'Widget' contains all edges and nodes that 'Selection' contained
   --      before the call.
   --    * 'Selection' intersection
   --        ( {E: Edge_Id | Contains (Widget, E)}
   --           union  {N: Node_Id | Contains (Widget, N)})
   --      = {Set of all edges and nodes that were
   --          inserted into 'Widget' during the call}
   procedure Insert_Selection_Difference
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Removes all edges and nodes in 'Selection' from 'Widget'
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Set of edges and nodes to be removed
   --  Postcondition:
   --    ( {E: Edge_Id | Contains (Widget, E)} union
   --      {N: Node_Id | Contains (Widget, N)} ) intersection 'Selection'
   --    = {}
   procedure Remove_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection);

   ----------------------------------------------------------------------------
   --  Removes all edges and nodes from 'Widget'
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Postcondition:
   --    ( {E: Edge_Id | Contains (Widget, E)} union
   --      {N: Node_Id | Contains (Widget, N)} )     = {}
   procedure Clear
     (Widget : access Graph_Widget_Record'Class);


   -----------------
   -- Action Mode --
   -----------------

   ----------------------------------------------------------------------------
   --  Enables action mode and sets cursor. During action mode the user can
   --  only move the visual area inside the graph widget an click onto the
   --  graph widget. After each such click the graph widget emits a signal.
   --  See Giant.Graph_Widgets.Handlers for details.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Cursor - The cursor to be displayed during action mode
   procedure Start_Action_Mode
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   ----------------------------------------------------------------------------
   --  If the graph widget is in action mode, then cancels action mode and
   --  sets the cursor back to the graph widget's default cursor or to the
   --  waiting cursor. Does nothing otherwise.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Cancel_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Checks if the graph widget is in action mode.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    True if the graph widget is in action mode, False otherwise.
   function Is_Action_Mode_Active
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   ------------
   -- Layout --
   ------------

   ----------------------------------------------------------------------------
   --  Raised whenever a layout-modifying subprogram is called without the
   --  correct lock.
   Illegal_Lock_State : exception;

   ----------------------------------------------------------------------------
   --  Acquires a global lock for all nodes contained in 'Widget'. The owner
   --  of that lock may modify the layout on all nodes. The graph widget
   --  will stop displaying its content until the lock is released.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Lock   - The lock handle
   procedure Lock_All_Content
     (Widget    : access Graph_Widget_Record'Class;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Acquires a lock for a given selection. The owner of that lock may
   --  modify the layout on all nodes in that selection. The graph widget
   --  will stop displaying its content until the lock is released.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The selection to be locked
   --    Lock      - The lock handle
   --  Precondition:
   --    'Selection' is a subset of {E: Edge_Id | Contains (Widget, E)} union
   --                                {N: Node_Id | Contains (Widget, N)}
   --  Raises:
   --    * Unknown_Edge_Id if Precondition not satisfied
   --    * Unknown_Node_Id if Precondition not satisfied
   procedure Lock_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type);

   ----------------------------------------------------------------------------
   --  Modifies the position of one node.
   --
   --  Parameters:
   --    Widget   - The graph widget containing 'Node'
   --    Node     - The node to be moved
   --    Location - The new Position of the top middle point of 'Node'
   --    Lock     - The Lock handle previously acquired for a 'Node'
   --  Precondition:
   --    'Contains (Widget, Node)' and 'Lock' locks 'Node'
   --  Postcondition:
   --    'Get_Top_Middle (Widget, Node)' = 'Location'
   --  Raises:
   --    * Unknown_Node_Id if not 'Contains (Widget, Node)'
   --    * Illegal_Lock_State if 'Node' is not locked by 'Lock'
   procedure Set_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id;
      Location  : in     Vis.Logic.Vector_2d;
      Lock      : in     Lock_Type);

   ----------------------------------------------------------------------------
   --  Gets the position of one node.
   --
   --  Parameters:
   --    Widget - The graph widget containing 'Node'
   --    Node   - A node
   --  Returns:
   --    The top middle point of node in logical vector space
   --  Precondition:
   --    'Contains (Widget, Node)'
   --  Raises:
   --    Unknown_Node_Id if Precondition not satisfied
   function Get_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Vis.Logic.Vector_2d;

   ----------------------------------------------------------------------------
   --  Discards a lock previously acquired by 'Lock_All_Content' or
   --  'Lock_Selection'. That lock must not be used anymore.
   --  The graph widget will display all changes made to the layout.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Lock   - The lock to be destroyed
   procedure Release_Lock
     (Widget    : access Graph_Widget_Record'Class;
      Lock      : in     Lock_Type);


   ---------------------------------------
   -- Layout manipulations without lock --
   ---------------------------------------

   ----------------------------------------------------------------------------
   --  Shifts nodes aside in order to open an empty space.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Center - The center point of the empty space
   --    Width  - The width of the empty space
   --    Height - The height of the empty space
   procedure Make_Room
     (Widget    : access Graph_Widget_Record'Class;
      Center    : in     Vis.Logic.Vector_2d;
      Width     : in     Vis.Logic_Float;
      Height    : in     Vis.Logic_Float);

   ----------------------------------------------------------------------------
   --  Moves all nodes in a selection by a given vector.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Nodes to be moved
   --    Move      - Direction and length to be moved
   procedure Move_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Move      : in     Vis.Logic.Vector_2d);


   --------------------------
   -- Visualization Styles --
   --------------------------

   ----------------------------------------------------------------------------
   --  Get the Visualization style the graph widget is currently displayed
   --  with.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    The Visualization style
   function Get_Vis_Style
     (Widget     : access Graph_Widget_Record'Class)
      return Config.Vis_Styles.Visualisation_Style_Access;

   ----------------------------------------------------------------------------
   --  Set the Visualization style the graph widget is to be displayed with
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Style  - The visualization style
   procedure Set_Vis_Style
     (Widget     : access Graph_Widget_Record'Class;
      Style      : in     Config.Vis_Styles.Visualisation_Style_Access);


   ------------------
   -- Highlighting --
   ------------------

   ----------------------------------------------------------------------------
   --  Add a highlight color to all edges and nodes in a selection.
   --  If the content of that selection changes, the highlighting must be
   --  removed manually from all deleted edges and nodes. The highlighting
   --  must be added manually to all new edges and nodes.
   --  The graph widget does not remember the selection after the call
   --  to this subprogram is complete. Highlighting is an attribute specific
   --  to single edges and nodes, not to selections.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Add_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Global_Data.Selection_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Remove a highlight color from all edges and nodes in a selection.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Remove_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Global_Data.Selection_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Add a highlight color to all edges and nodes in a subgraph.
   --  If the content of that subgraph changes, the highlighting must be
   --  removed manually from all deleted edges and nodes. The highlighting
   --  must be added manually to all new edges and nodes.
   --  The graph widget does not remember the subgraph after the call
   --  to this subprogram is complete. Highlighting is an attribute specific
   --  to single edges and nodes, not to selections.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Add_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Global_Data.Subgraph_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Removes a highlight color from all the edges and nodes in a subgraph.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The edges and nodes to be highlighted
   --    Color     - The highlight color to be added to 'Selection's contents
   procedure Remove_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Global_Data.Subgraph_High_Light_ID);

   ----------------------------------------------------------------------------
   --  Deletes all highlight colors from all the edges and all nodes in
   --  'Widget'
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Clear_Highlighting
     (Widget     : access Graph_Widget_Record'Class);


   -----------------------
   -- Visual Attributes --
   -----------------------

   ----------------------------------------------------------------------------
   --  Checks if an edge is hidden.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The edge
   --  Returns:
   --    True if 'Edge' is hidden, False else
   --  Precondition:
   --    'Contains (Widget, Edge)'
   --  Raises:
   --    Unknown_Edge_Id if Precondition is not satisfied
   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Checks if a node is hidden.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - The node to be checked
   --  Returns:
   --    True if 'Node' is hidden, False else
   --  Precondition:
   --    'Contains (Widget, Node)'
   --  Raises:
   --    Unknown_Node_Id if Precondition is not satisfied
   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Sets the "hidden" status of all edges and all nodes in 'Selection'.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - Container of edges and nodes whose status is to be set
   --    Hidden    - Desired result of status change
   --  Precondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Contains (Widget, E)'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Contains (Widget, N)'
   --  Postcondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Is_Hidden (Widget, E)' = 'Hidden'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Is_Hidden (Widget, N)' = 'Hidden'
   --  Raises:
   --    * Unknown_Edge_Id if Precondition is not satisfied
   --    * Unknown_Node_Id if Precondition is not satisfied
   procedure Set_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Hidden     : in     Boolean);

   ----------------------------------------------------------------------------
   --  Unhides all nodes and all edges in 'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Postcondition:
   --    * For all E: Edge_Id:
   --        'Contains (Widget, E)' ==> not 'Is_Hidden (Widget, E)'
   --    * For all N: Node_Id:
   --        'Contains (Widget, N)' ==> not 'Is_Hidden (Widget, N)'
   procedure Unhide_All
     (Widget     : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Must be called when an annotation is created or destroyed. Updates
   --  the display, so the user can determine whether a 'Node' is annotated
   --  or not.
   --  This subprogram only needs to be called if the status of an already
   --  inserted node is changed. If a node is newly inserted, then the graph
   --  widget will be check its status automatically.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - A node with a new annotation or a node whose annotation was
   --             deleted
   --  Precondition:
   --    'Contains (Widget, Node)'
   --  Raises:
   --    Unknown_Node_Id if Precondition is not satisfied
   procedure Change_Annotation_State
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id);


   ------------------------
   -- Zooming and Moving --
   ------------------------

   ----------------------------------------------------------------------------
   --  Returns the greatest zoom level applicable for 'Widget'. This is
   --  limited by the maximum range of internal variables.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Maximum zoom level
   function Get_Maximum_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level;

   ----------------------------------------------------------------------------
   --  Returns the current zoom level within 'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Current zoom level
   function Get_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level;

   ----------------------------------------------------------------------------
   --  Sets the current zoom level in 'Widget'. If 'Zoom' is greater than
   --  Max = 'Get_Maximum_Zoom_Level (Widget)' then Max is used instead. If
   --  'Zoom' < 0.0 then 0.0 is used.
   --  If the zoom level is set to 0.0, then the center point of the visible
   --  area is set to 0.0. If after the application of the new zoom level, the
   --  center point of the visible area will be lying outside of the possible
   --  range, then the center point is adjusted to a value close to its
   --  old value.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Zoom   - The desired zoom level
   procedure Set_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Zoom       : in     Vis.Zoom_Level);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so that the nodes incident to 'Edge'
   --  fill 'Widget' and are both located within the visible area. If the
   --  zoom level needed for that purpose is too great, then the maximum
   --  zoom level will be used and 'Edge' will be centered.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The Edge to be centered
   --  Precondition:
   --    'Contains (Widget, Edge)'
   --  Raises:
   --    Unknown_Edge_Id if Precondition is not satisfied
   procedure Zoom_To_Edge
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so 'Selection' fills 'Widget'
   --  and all nodes in 'Selection' are located within the visible area. If the
   --  necessary zoom level is too great, then the maximum zoom level will
   --  be used and 'Selection' will be centered. If 'Selection' is empty then
   --  nothing is done.
   --
   --  Parameters:
   --    Widget    - The graph widget
   --    Selection - The Selection to be zoomed onto
   --  Precondition:
   --    * For all E: Edge_Id:
   --        E contained in 'Selection' ==> 'Contains (Widget, E)'
   --    * For all N: Node_Id:
   --        N contained in 'Selection' ==> 'Contains (Widget, N)'
   --  Raises:
   --    * Unknown_Edge_Id if Precondition is not satisfied
   --    * Unknown_Node_Id if Precondition is not satisfied
   procedure Zoom_To_Selection
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection);

   ----------------------------------------------------------------------------
   --  Changes zoom level and location so that all content fills 'Widget'
   --  and all nodes are located within the visible area. If the
   --  zoom level needed for that purpose is too great, then the maximum
   --  zoom level will be used and the content will be centered. If there
   --  is no content in 'Widget' then nothing is done.
   --
   --  Parameters:
   --    Widget - The graph widget
   procedure Zoom_To_All
     (Widget     : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Returns the center of the visible area currently visible within
   --  'Widget'.
   --
   --  Parameters:
   --    Widget - The graph widget
   --  Returns:
   --    Center point of the visible area
   function Get_Location
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Vector_2d;

   ----------------------------------------------------------------------------
   --  Sets the center point of the visible area in 'Widget'. If zoom level
   --  is 0.0 then nothing is done. If 'Location' is outside of the
   --  displayable area then a point close to 'Location' is chosen.
   procedure Set_Location
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d);

   ----------------------------------------------------------------------------
   --  Sets the zoom level in 'Widget' and then the center point of the
   --  visible area in 'Widget'.
   --  If 'Zoom' is greater than Max = 'Get_Maximum_Zoom_Level (Widget)'
   --  then Max is used instead. If 'Zoom' < 0.0 then 0.0 is used.
   --  If 'Location' is outside of the displayable area then a point close
   --  to 'Location' is chosen. If the zoom level is set to 0.0 then the
   --  center point of the visible area is set to 0.0. Else the center point
   --  is set to 'Location'.
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Location - Desired center point
   --    Zoom     - Desired zoom level
   procedure Set_Location_And_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d;
      Zoom       : in     Vis.Zoom_Level);


   ----------------------------------------------------------------------------
   --  Style for drawing of edges
   --  can be 'Continouus_Line', 'Dashed_Line', 'Dotted_Line'
   type Edge_Style_Type is new Config.Vis_Styles.Edge_Line_Style;

                           ------------------
private                    -- private part --
                           ------------------

   ----------------------------------------------------------------------------
   --  Hashmap that maps Graph_Lib.Edge_Id to Vis_Data.Vis_Edge_Id
   package Edge_Id_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Edge_Id,
      Hash       => Graph_Lib.Hash_Edge_Id,
      Value_Type => Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Hashmap that maps Graph_Lib.Node_Id to Vis_Data.Vis_Node_Id
   package Node_Id_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Node_Id,
      Hash       => Graph_Lib.Hash_Node_Id,
      Value_Type => Vis_Data.Vis_Node_Id);

   ----------------------------------------------------------------------------
   --  not yet implemented
   type Lock_Type is null record;


   -------------
   -- Drawing --
   -------------

   --------------
   -- Settings --
   --------------

   ----------------------------------------------------------------------------
   --  Mapping Config.Color_Access --> allocated color
   package Color_Mappings is new Hashed_Mappings
     (Key_Type   => Config.Color_Access,
      Hash       => Config.Hash_Color_Access,
      Value_Type => Gdk.Color.Gdk_Color);

   type Settings_Type is
      record
         --  Colors used for drawing. All those colors are allocated in the
         --  default 'Colormap'.
         Color_Pool : Color_Mappings.Mapping;
      end record;


   -------------------------
   -- Graph_Widget_Record --
   -------------------------

   ----------------------------------------------------------------------------
   --  The one and only graph widget tagged type
   type Graph_Widget_Record is new Gtk.Widget.Gtk_Widget_Record with
      record
         Settings : Settings_Type;

         All_Nodes          : Vis_Data.Vis_Node_Sets.Set;
         Node_Map           : Node_Id_Mappings.Mapping;
         Highest_Node_Layer : Vis_Data.Layer_Pool;
         All_Edges          : Vis_Data.Vis_Edge_Sets.Set;
         Edge_Map           : Edge_Id_Mappings.Mapping;
         Highest_Edge_Layer : Vis_Data.Layer_Pool;
         Manager            : Vis_Data.Region_Manager;
      end record;

end Giant.Graph_Widgets;
