------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets-notifications.ads,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------
--
--  This Package manages the notification mechanism in a graph widget.
--  The graph widget calles the subprograms in this package. These
--  subprograms then forward the notifications to the graph widget's clients.
--


with Gdk.Event;

package Giant.Graph_Widgets.Notifications is


   -------------------
   -- PopUp Menus --
   -------------------

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has requested a PopUp Menu for
   --  the background
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Event - The gdk source event
   --    Location - The location where the event happened in logical
   --               coordinates.
   procedure Background_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d);

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has requested a PopUp Menu for
   --  one specific edge.
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Event    - The gdk source event
   --    Location - Position where the mouse was pressed
   --    Edge     - The edge, the PopUp Menu is requested for
   procedure Edge_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has requested a PopUp Menu for
   --  one specific node.
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Event    - The gdk source event
   --    Location - Position where the mouse was pressed
   --    Node     - The node, the PopUp Menu is requested for
   procedure Node_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id);


   -----------------------
   -- Selection Changes --
   -----------------------

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has given the command to modify
   --  the current selection.
   --
   --  Parameters:
   --    Widget     - The graph widget
   --    Action     - The user's change request
   --    Difference - The Selection necessary to service the request:
   --                 * Insert --> edges and nodes to be inserted
   --                 * Remove --> edges and nodes to be removed
   --                 * Change --> new content
   --                 * Clear  --> null
   procedure Selection_Changed
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection);


   -----------------
   -- Action Mode --
   -----------------

   ----------------------------------------------------------------------------
   --  Emits the 'Graph_Widgets.Handlers.Action_Mode_Button_Press_Event'
   --  to inform any listener that the user has performed a
   --  "button_press_event" on empty background while the graph widget was
   --  in action mode.
   --
   --  Note that the graph widget remains in action mode after this signal.
   --  If action mode should be canceled, call 'Cancel_Action_Mode'
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Event    - The event obtained from GtkAda. See 'Gdk.Event' for
   --               details.
   --    Location - The location where the event happened in logical
   --               coordinates.
   procedure Action_Mode_Button_Press_Event_Background
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d);

   ----------------------------------------------------------------------------
   --  Emits the 'Graph_Widgets.Handlers.Action_Mode_Button_Press_Event'
   --  to inform any listener that the user has performed a
   --  "button_press_event" on an edge while the graph widget was in
   --  action mode.
   --
   --  Note that the graph widget remains in action mode after this signal.
   --  If action mode should be canceled, call 'Cancel_Action_Mode'
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Event    - The event obtained from GtkAda. See 'Gdk.Event' for
   --               details.
   --    Location - The location where the event happened in logical
   --               coordinates.
   --    Edge     - The edge on that the button was pressed
   procedure Action_Mode_Button_Press_Event_Edge
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id);

   ----------------------------------------------------------------------------
   --  Emits the 'Graph_Widgets.Handlers.Action_Mode_Button_Press_Event'
   --  to inform any listener that the user has performed a
   --  "button_press_event" on a node while the graph widget was in
   --  action mode.
   --
   --  Note that the graph widget remains in action mode after this signal.
   --  If action mode should be canceled, call 'Cancel_Action_Mode'
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Event    - The event obtained from GtkAda. See 'Gdk.Event' for
   --               details.
   --    Location - The location where the event happened in logical
   --               coordinates.
   --    Node     - The node on that the button was pressed
   procedure Action_Mode_Button_Press_Event_Node
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id);


   --------------------------
   -- Visible Area Changes --
   --------------------------

   ----------------------------------------------------------------------------
   --  Emits the Graph_Widgets.Handlers.Logical_Area_Changed_Signal
   --  to inform any listener thant the total logical area occupied by
   --  the graph has changed.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The logical area occupied by the graph in 'Widget'
   procedure Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Emits the Graph_Widgets.Handlers.Visible_Area_Changed_Signal
   --  to inform any listener that the visible logical area has changed.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The visible area inside 'Widget'
   procedure Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);


end Giant.Graph_Widgets.Notifications;
