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
--  $RCSfile: giant-graph_widgets-callbacks.ads,v $, $Revision: 1.10 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------


with Gdk.Event;
with Gtk.Adjustment;
with Gtk.Widget;
pragma Elaborate_All (Gtk.Widget);

package Giant.Graph_Widgets.Callbacks is

   ----------------------------------------------------------------------------
   --  Connects all Callbacks 'Widget' needs
   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class);

   procedure Update_Scrollbars
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Updates a user action so it respects newly appeared edges. This must
   --  be called when new edges are shown on the graph widget, e.g. when
   --  nodes come into view.
   procedure Edges_Appeared
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Data.Vis_Edge_Sets.Set);

   ----------------------------------------------------------------------------
   --  Updates the position of the mouse pointer. This must be called whenever
   --  the position of the mouse pointer changes relative to the graph, e.g.
   --  when the widget scrolls.
   procedure Mouse_Pointer_Moved_To
     (Widget          : access Graph_Widget_Record'Class;
      Motion_Position : in     Vis.Absolute.Vector_2d);

private

   procedure On_Horizontal_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget);

   procedure On_Vertical_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget);

   procedure On_Realize
     (Widget : access Graph_Widget_Record'Class);

   procedure After_Realize
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Unrealize
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Destroy
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Set_Scroll_Adjustments
     (Widget     : access Graph_Widget_Record'Class;
      Horizontal : in     Gtk.Adjustment.Gtk_Adjustment;
      Vertical   : in     Gtk.Adjustment.Gtk_Adjustment);

   procedure On_Size_Request
     (Widget      : access Graph_Widget_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access);

   procedure On_Size_Allocate
     (Widget     : access Graph_Widget_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access);

   function On_Expose_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean;

   function On_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   function On_Button_Release_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   function On_Motion_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean;

   function On_Enter_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean;

   function On_Leave_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean;

   function On_Auto_Scroll_Timeout
     (Widget : in     Graph_Widget)
     return Boolean;

   Left_Button   : constant := 1;
   Middle_Button : constant := 2;
   Right_Button  : constant := 3;

   --  Points the mouse cursor must be moved to activate a drag
   Default_Click_Distance_Tolerance : constant := 3;
   --  Milliseconds between two auto scroll steps
   Default_Auto_Scroll_Delay        : constant := 100;
   --  Points the mouse cursor must be moved to reach the next speed level
   Default_Auto_Scroll_Sensitivity  : constant := 3;
   --  Points the move offset is increased in every acceleration level
   Default_Auto_Scroll_Acceleration : constant := 10;

end Giant.Graph_Widgets.Callbacks;
