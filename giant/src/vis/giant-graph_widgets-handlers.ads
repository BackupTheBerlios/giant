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
--  $RCSfile: giant-graph_widgets-handlers.ads,v $, $Revision: 1.4 $
--  $Author: keulsn $
--  $Date: 2003/06/29 13:56:08 $
--
------------------------------------------------------------------------------
--
--  This Package provides Callback and Marshaller instances necessary to
--  connect handlers to the signals emitted by 'Graph_Widgets.Graph_Widget'.
--


with Gdk.Event;
with Gtk.Arguments;
with Gtk.Handlers;
with Gtkada.Types;

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
   --  "button_press_event" on the graph widget while it was in action mode
   type Button_Press_Action is
      record
         --  The event given through "button_press_event"
         Event    : Gdk.Event.Gdk_Event_Button;
         --  The logical location where the event happened in the graph
         Location : Vis.Logic.Vector_2d;
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
   --    The 'Button_Press_Action' at index 'Num' in 'Args'
   function To_Button_Press_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Button_Press_Action;


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
     (Widget      : access Graph_Widget_Record'Class;
      User_Action : in     Button_Press_Action);

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

   function Get_Signal_Array
     return Gtkada.Types.Chars_Ptr_Array;


end Giant.Graph_Widgets.Handlers;
