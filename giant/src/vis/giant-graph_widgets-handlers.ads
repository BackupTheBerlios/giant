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
--  $RCSfile: giant-graph_widgets-handlers.ads,v $, $Revision: 1.9 $
--  $Author: keulsn $
--  $Date: 2003/07/22 18:21:32 $
--
------------------------------------------------------------------------------
--
--  This Package provides Callback and Marshaller instances necessary to
--  connect handlers to the signals emitted by 'Graph_Widgets.Graph_Widget'.
--


with Gdk.Event;
with Gtk.Arguments;
with Gtk.Handlers;
with Gtk.Object;
with Gtkada.Types;

with Giant.Graph_Lib;

package Giant.Graph_Widgets.Handlers is

   ----------------------------------
   -- Signals without return value --
   ----------------------------------

   package Graph_Widget_Callbacks is new Gtk.Handlers.Callback
     (Widget_Type => Graph_Widget_Record);


   --------------------
   -- Argument types --
   --------------------

   --  The gtk+ functions "gtk_signal_emitv" and "gtk_signal_emitv_by_name"
   --  are not imported into GtkAda. Therefore it is hard to see, how
   --  one can have signals with more than one argument. Therefore
   --  multiple arguments are packed into records.


   ----------------------------------------------------------------------------
   --  "edge_popup_event" on an edge within a graph widget
   type Edge_Popup_Action is
      record
         Event    : Gdk.Event.Gdk_Event_Button;
         Edge     : Graph_Lib.Edge_Id;
      end record;

   ----------------------------------------------------------------------------
   --  "node_popup_event" on an edge within a graph widget
   type Node_Popup_Action is
      record
         Event    : Gdk.Event.Gdk_Event_Button;
         Node     : Graph_Lib.Node_Id;
      end record;

   ----------------------------------------------------------------------------
   --  "button_press_event" on the graph widget while it was in action mode
   type Button_Press_Action is
      record
         --  The event given through "button_press_event"
         Event    : Gdk.Event.Gdk_Event_Button;
         --  The logical location where the event happened in the graph
         Location : Vis.Logic.Vector_2d;
      end record;

   ----------------------------------------------------------------------------
   --  "selection_changed_signal on the graph widget"
   type Selection_Change_Action is
      record
         --  Way in which the current selection has changed
         Action     : Selection_Change_Type;
         --  New selection or difference between old and modified selection
         Difference : Graph_Lib.Selections.Selection;
      end record;


   -----------------
   -- Conversions --
   -----------------

   ----------------------------------------------------------------------------
   --  Conversion function for Marshallers
   --
   --  Parameters:
   --    Args - The GtkAda argument array
   --    Num  - The Index of an argument in 'Args'
   --  Returns:
   --    The Area at index 'Num' in 'Args'
   function To_Rectangle_2d
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Vis.Logic.Rectangle_2d;

   ----------------------------------------------------------------------------
   --  Conversion function for Marshallers
   --
   --  Parameters
   --    Args - The GtkAda argument array
   --    Num  - The Index of an argument in 'Args'
   --  Returns:
   --    The 'Edge_Popup_Action' at index 'Num' in 'Args'
   function To_Edge_Popup_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Edge_Popup_Action;

   ----------------------------------------------------------------------------
   --  Conversion function for Marshallers
   --
   --  Parameters
   --    Args - The GtkAda argument array
   --    Num  - The Index of an argument in 'Args'
   --  Returns:
   --    The 'Node_Popup_Action' at index 'Num' in 'Args'
   function To_Node_Popup_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Node_Popup_Action;

   ----------------------------------------------------------------------------
   --  Conversion function for Marshallers
   --
   --  Parameters
   --    Args - The GtkAda argument array
   --    Num  - The Index of an argument in 'Args'
   --  Returns:
   --    The 'Button_Press_Action' at index 'Num' in 'Args'
   function To_Button_Press_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Button_Press_Action;

   ----------------------------------------------------------------------------
   --  Conversion function for Marshallers
   --
   --  Parameters
   --    Args - The GtkAda argument array
   --    Num  - The Index of an argument in 'Args'
   --  Returns:
   --    The 'Selection_Change_Action' at index 'Num' in 'Args'
   function To_Selection_Change_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Selection_Change_Action;


   ------------------------------
   -- "background_popup_event" --
   ------------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the user has requested a pop up-menu on the background
   --  of a graph widget
   Background_Popup_Event : constant String :=
     "background_popup_event";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Background_Popup_Event
   --  Parameters:
   --    Widget - The graph widget
   --    Action - The user action inside 'Widget'
   type Background_Popup_Event_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Button_Press_Action);

   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Background_Popup_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Background_Popup_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d);


   ------------------------
   -- "edge_popup_event" --
   ------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the user has requested a pop up-menu on an edge
   Edge_Popup_Event : constant String :=
     "edge_popup_event";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Edge_Popup_Event
   --  Parameters:
   --    Widget - The graph widget
   --    Action - The user action inside 'Widget'
   type Edge_Popup_Event_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Edge_Popup_Action);

   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Edge_Popup_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Edge_Popup_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button;
      Edge   : in     Graph_Lib.Edge_Id);


   ------------------------
   -- "node_popup_event" --
   ------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the user has requested a pop up-menu on a node
   Node_Popup_Event : constant String :=
     "node_popup_event";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Node_Popup_Event
   --  Parameters:
   --    Widget - The graph widget
   --    Action - The user action inside 'Widget'
   type Node_Popup_Event_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Node_Popup_Action);

   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Node_Popup_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Node_Popup_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button;
      Node   : in     Graph_Lib.Node_Id);


   -----------------------
   -- Selection Changes --
   -----------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the current selection has changed in a graph widget
   Selection_Change_Signal : constant String :=
     "selection_change_signal";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Node_Popup_Event
   --  Parameters:
   --    Widget - The graph widget
   --    Action - The user action inside 'Widget'
   type Selection_Change_Signal_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Selection_Change_Action);

   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Selection_Change_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Selection_Change_Signal
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection);


   --------------------------------------
   -- "action_mode_button_press_event" --
   --------------------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the user has performed a
   --  "button_press_event" while the graph widget was in action mode.
   Action_Mode_Button_Press_Event : constant String :=
     "action_mode_button_press_event";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Action_Mode_Button_Press_Event
   --  Parameters:
   --    Widget - The graph widget
   --    Action - The user action inside 'Widget'
   type Action_Mode_Button_Press_Event_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Button_Press_Action);

   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Action_Mode_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Action_Mode_Button_Press_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d);


   ----------------------------
   -- "logical_area_changed" --
   ----------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the logical area containing the complete graph
   --  changes. This change can be due to inserting, removing of nodes
   --  or to moving nodes inside the Graph_Widget.
   Logical_Area_Changed_Signal : constant String := "logical_area_changed";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Visible_Area_Changed_Signal
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The visible area inside 'Widget'
   type Logical_Area_Changed_Signal_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Package providing the 'Connect' subprograms if no user data is needed.
   package Logical_Area_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);


   ----------------------------
   -- "visible_area_changed" --
   ----------------------------

   ----------------------------------------------------------------------------
   --  Emitted whenever the visible logical area changes. This change can
   --  be due to resizing of the widget, moving over the logical area,
   --  changing the zoom level
   Visible_Area_Changed_Signal : constant String := "visible_area_changed";

   ----------------------------------------------------------------------------
   --  Type of handlers for signal Visible_Area_Changed_Signal
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The visible area inside 'Widget'
   type Visible_Area_Changed_Signal_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Package providing the 'Connect' subprograms, if no user data is needed.
   package Visible_Area_Cbs renames Graph_Widget_Callbacks;

   ----------------------------------------------------------------------------
   --  Emits the signal.
   procedure Emit_Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);


   -----------------
   -- All Signals --
   -----------------

   ----------------------------------------------------------------------------
   --  Signal to be emitted when the Gdk.Scrolled_Window sets new
   --  Gtk.Adjustments
   Set_Scroll_Adjustments_Signal : constant String := "set_scroll_adjustments";

   --  value is constant
   function Get_Signal_Array
     return Gtkada.Types.Chars_Ptr_Array;

   --  parameters for the signals in the array returned by 'Get_Signal_Array'
   function Get_Signal_Parameters
     return Gtk.Object.Signal_Parameter_Types;

   --  returns the index for the "set_scroll_adjustments" signal in the
   --  array returned by 'Get_Signal_Array'
   function Get_Scroll_Adjustments_Signal
     return Glib.Guint;

end Giant.Graph_Widgets.Handlers;
