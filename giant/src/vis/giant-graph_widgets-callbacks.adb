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
--  $RCSfile: giant-graph_widgets-callbacks.adb,v $, $Revision: 1.13 $
--  $Author: keulsn $
--  $Date: 2003/07/22 00:11:23 $
--
------------------------------------------------------------------------------


with System;

with Gdk.Main;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;
with Gdk.Window_Attr;
with Gtk.Arguments;
with Gtk.Enums;
with Gtk.Handlers;
--with Gtk.Main;
with Gtk.Style;

with Giant.Graph_Widgets.Drawing;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Widgets.Positioning;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;

package body Giant.Graph_Widgets.Callbacks is


   package Callbacks_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Callbacks");

   function "or"
     (Left  : in Gdk.Types.Gdk_Event_Mask;
      Right : in Gdk.Types.Gdk_Event_Mask)
     return Gdk.Types.Gdk_Event_Mask
     renames Gdk.Types."or";

   Handled_Event_Mask : constant Gdk.Types.Gdk_Event_Mask  :=
     Gdk.Types.Button_Press_Mask or
     Gdk.Types.Button_Release_Mask or
     Gdk.Types.Button1_Motion_Mask;


   --------------------
   -- Mouse handling --
   --------------------

   function Interpret_Modifiers
     (Modifiers : in     Gdk.Types.Gdk_Modifier_Type)
     return Selection_Modify_Type is

      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      if (Modifiers and Gdk.Types.Shift_Mask) /= 0 then
         return Toggle;
      elsif (Modifiers and Gdk.Types.Control_Mask) /= 0 then
         return Add;
      else
         return Change;
      end if;
   end Interpret_Modifiers;

   --  Parameters:
   --    Restrict_Change - If set to True then 'Mode = Change' will be ignored
   --                      if performed on an edge or a node that is
   --                      already selected.
   procedure Process_Mouse_Click
     (Widget          : access Graph_Widget_Record'Class;
      Mode            : in     Selection_Modify_Type;
      Restrict_Change : in     Boolean := False) is

      Edge : Vis_Data.Vis_Edge_Id := States.Get_Click_Edge (Widget);
      Node : Vis_Data.Vis_Node_Id := States.Get_Click_Node (Widget);
   begin
      if Vis_Data."/=" (Node, null) then
         if Mode /= Change or else
          (not Restrict_Change or else not Is_Node_In_Selection (Widget, Node))
         then
            Modify_Selection_With_Node_And_Notify (Widget, Node, Mode);
         end if;
      elsif Vis_Data."/=" (Edge, null) then
         if Mode /= Change or else
          (not Restrict_Change or else not Is_Edge_In_Selection (Widget, Edge))
         then
            Modify_Selection_With_Edge_And_Notify (Widget, Edge, Mode);
         end if;
      else
         if Mode = Change then
            Clear_Selection_And_Notify (Widget);
         end if;
      end if;
   end Process_Mouse_Click;

   procedure Cancel_Click
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.End_Click (Widget);
   end Cancel_Click;

   procedure Begin_Mouse_Move_Action
     (Widget : access Graph_Widget_Record'Class;
      Mode   : in     Selection_Modify_Type) is

      Iterator : Vis_Node_Sets.Iterator;
      Node     : Vis_Data.Vis_Node_Id;
   begin
      Process_Mouse_Click
        (Widget          => Widget,
         Mode            => Mode,
         Restrict_Change => True);

      if Vis_Data."/=" (States.Get_Click_Node (Widget), null) then
         --  Drag
         States.Begin_Drag (Widget);
         States.Changed_Temporary (Widget);
         --  Remove moved nodes from region manager
         States.Changed_Visual (Widget);
         Iterator := Vis_Node_Sets.Make_Iterator (Get_Floating_Nodes (Widget));
         while Vis_Node_Sets.More (Iterator) loop
            Vis_Node_Sets.Next (Iterator, Node);
            Vis_Data.Drop_Node (Widget.Manager, Node);
         end loop;
         Vis_Node_Sets.Destroy (Iterator);
         Redraw (Widget);
      elsif Vis_Data."/=" (States.Get_Click_Edge (Widget), null) then
         --  Drag on edge --> ignore
         States.End_Click (Widget);
      else
         --  Rectangle
         States.Begin_Rectangle (Widget);
      end if;
   end Begin_Mouse_Move_Action;

   procedure Drop
     (Widget : access Graph_Widget_Record'Class) is

      Lock : Lock_Type;
   begin
      --  user has dropped some nodes
      Lock_All_Content (Widget, Lock);
      Move_Nodes
        (Widget => Widget,
         Nodes  => Get_Floating_Nodes (Widget),
         Offset => Positioning.Get_Logic
                     (Widget, States.Get_Mouse_Move_Distance (Widget)));
      States.End_Drag (Widget);
      States.Changed_Temporary (Widget);
      Release_Lock (Widget, Lock);
   end Drop;

   procedure Cancel_Drag
     (Widget : access Graph_Widget_Record'Class) is

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Move_Nodes
        (Widget => Widget,
         Nodes  => Get_Floating_Nodes (Widget),
         Offset => Vis.Logic.Zero_2d);
      States.End_Drag (Widget);
      States.Changed_Temporary (Widget);
      Release_Lock (Widget, Lock);
   end Cancel_Drag;


   --------------------------
   -- Explicit Marshallers --
   --------------------------

   procedure Set_Scroll_Adjustments_Handler
     (Widget : access Graph_Widget_Record'Class;
      Args   : in     Gtk.Arguments.Gtk_Args) is

      function To_Adjustment (Addr : in System.Address)
        return Gtk.Adjustment.Gtk_Adjustment is

         Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
      begin
         return Gtk.Adjustment.Gtk_Adjustment
           (Gtk.Get_User_Data (Addr, Stub));
      end To_Adjustment;

      H_Adj : Gtk.Adjustment.Gtk_Adjustment := To_Adjustment
        (Gtk.Arguments.Get_Nth (Args, 1));
      V_Adj : Gtk.Adjustment.Gtk_Adjustment := To_Adjustment
        (Gtk.Arguments.Get_Nth (Args, 2));
   begin
      On_Set_Scroll_Adjustments (Widget, H_Adj, V_Adj);
   end Set_Scroll_Adjustments_Handler;


   -----------------------
   -- Connect Callbacks --
   -----------------------

   package Graph_Widget_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Graph_Widget_Record);
   package Graph_Widget_Boolean_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Graph_Widget_Record,
      Return_Type => Boolean);

   package Realize_Cbs renames Graph_Widget_Callback;

   package Unrealize_Cbs renames Graph_Widget_Callback;

   package Set_Scroll_Adjustment_Cbs renames Graph_Widget_Callback;
   --  cannot provide marshaller instanciation because GtkAda supports
   --  only one argument per signal

   package Size_Request_Cbs renames Graph_Widget_Callback;
   package Requisition_Marshallers is new
     Size_Request_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Requisition_Access,
        Conversion => Gtk.Arguments.To_Requisition);

   package Size_Allocate_Cbs renames Size_Request_Cbs;
   package Allocation_Marshallers is new
     Size_Allocate_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Allocation_Access,
        Conversion => Gtk.Arguments.To_Allocation);

   package Expose_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Destroy_Cbs renames Graph_Widget_Callback;

   package Button_Press_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Button_Release_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Motion_Notify_Event_Cbs renames Graph_Widget_Boolean_Callback;


   package Realize_Handling is new Gtk.Widget.Realize_Handling
     (Widget_Type  => Graph_Widget_Record,
      Realize_Proc => On_Realize);


   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Set_Events
        (Widget,
         Get_Events (Widget) or Handled_Event_Mask);

      Realize_Cbs.Connect
        (Widget => Widget,
         Name   => "realize",
         Marsh  => Realize_Cbs.To_Marshaller (After_Realize'Access),
         After  => True);
      Unrealize_Cbs.Connect
        (Widget => Widget,
         Name   => "unrealize",
         Marsh  => Unrealize_Cbs.To_Marshaller (On_Unrealize'Access));
      Destroy_Cbs.Connect
        (Widget => Widget,
         Name   => "destroy",
         Marsh  => Destroy_Cbs.To_Marshaller (On_Destroy'Access));
      Set_Scroll_Adjustment_Cbs.Connect
        (Widget => Widget,
         Name   => Handlers.Set_Scroll_Adjustments_Signal,
         Cb     => Set_Scroll_Adjustments_Handler'Access);
      Size_Request_Cbs.Connect
        (Widget => Widget,
         Name   => "size_request",
         Marsh  => Requisition_Marshallers.To_Marshaller
                     (On_Size_Request'Access));
      Size_Allocate_Cbs.Connect
        (Widget => Widget,
         Name   => "size_allocate",
         Marsh  => Allocation_Marshallers.To_Marshaller
                     (On_Size_Allocate'Access));
      Expose_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "expose_event",
         Marsh  => Expose_Event_Cbs.To_Marshaller (On_Expose_Event'Access));
      Button_Press_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "button_press_event",
         Marsh  => Button_Press_Event_Cbs.To_Marshaller
                     (On_Button_Press_Event'Access));
      Button_Release_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "button_release_event",
         Marsh  => Button_Release_Event_Cbs.To_Marshaller
                     (On_Button_Release_Event'Access));
      Motion_Notify_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "motion_notify_event",
         Marsh  => Motion_Notify_Event_Cbs.To_Marshaller
                     (On_Motion_Notify_Event'Access));

      Realize_Handling.Set_Realize (Widget);
   end Connect_All_Callbacks;


   ---------------
   -- Callbacks --
   ---------------

   procedure On_Realize
     (Widget : access Graph_Widget_Record'Class) is

      Attributes      : Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : Gdk.Types.Gdk_Window_Attributes_Type;
      Window          : Gdk.Window.Gdk_Window;

      procedure Set_User_Data
        (Window : Gdk.Gdk_Window; Widget : System.Address);
      pragma Import (C, Set_User_Data, "gdk_window_set_user_data");

   begin
      Set_Flags (Widget, Gtk.Widget.Realized);

      Gdk.Window_Attr.Gdk_New
        (Window_Attr => Attributes,
         Event_Mask  => Gdk.Types."or"
                          (Get_Events (Widget), Gdk.Types.Exposure_Mask),
         X           => Get_Allocation_X (Widget),
         Y           => Get_Allocation_Y (Widget),
         Width       => Glib.Gint (Get_Allocation_Width (Widget)),
         Height      => Glib.Gint (Get_Allocation_Height (Widget)),
         Window_Type => Gdk.Types.Window_Child,
         Visual      => Get_Visual (Widget),
         Colormap    => Get_Colormap (Widget));

      Attributes_Mask :=
        Gdk.Types."or" (Gdk.Types."or" (Gdk.Types."or"
        (Gdk.Types.Wa_X, Gdk.Types.Wa_Y), Gdk.Types.Wa_Visual),
         Gdk.Types.Wa_Colormap);
      Gdk.Window.Gdk_New
        (Window,
         Gtk.Widget.Get_Window (Get_Parent (Widget)),
         Attributes,
         Attributes_Mask);
      Set_Window
        (Widget,
         Window);
      Set_Style
        (Widget,
         Gtk.Style.Attach (Get_Style (Widget), Get_Window (Widget)));
      Gtk.Style.Set_Background
        (Get_Style (Widget),
         Get_Window (Widget),
         Gtk.Enums.State_Active);

      Set_User_Data
        (Window,
         Gtk.Get_Object (Widget));
   end On_Realize;

   procedure After_Realize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Settings.Set_Up (Widget);
      Drawing.Set_Up (Widget);
      if States.Must_Flush_Locked_Content (Widget) then
         Flush_Locked (Widget);
      end if;
      Grab_Default (Widget);
      States.Realized (Widget);
   end After_Realize;

   procedure On_Unrealize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Drawing.Shut_Down (Widget);
      Settings.Shut_Down (Widget);
   end On_Unrealize;

   procedure On_Destroy
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Shut_Down_Graph_Widget (Widget);
   end On_Destroy;

   procedure On_Set_Scroll_Adjustments
     (Widget     : access Graph_Widget_Record'Class;
      Horizontal : in     Gtk.Adjustment.Gtk_Adjustment;
      Vertical   : in     Gtk.Adjustment.Gtk_Adjustment) is
   begin
      -----------------------------------raise Unimplemented;
      null;
   end On_Set_Scroll_Adjustments;

   procedure On_Size_Request
     (Widget      : access Graph_Widget_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access) is
   begin
      Requisition.Width  := Default_Width;
      Requisition.Height := Default_Height;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end On_Size_Request;

   procedure On_Size_Allocate
     (Widget     : access Graph_Widget_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access) is
   begin
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Gdk.Window.Move_Resize
           (Get_Window (Widget),
            Allocation.X,
            Allocation.Y,
            Glib.Gint (Allocation.Width),
            Glib.Gint (Allocation.Height));
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
      Resize_Graph_Widget
        (Widget,
         Vis.Absolute.Combine_Vector
           (X => Vis.Absolute_Int (Allocation.Width),
            Y => Vis.Absolute_Int (Allocation.Height)));
   end On_Size_Allocate;

   function On_Expose_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean is

      Gdk_Area   : Gdk.Rectangle.Gdk_Rectangle;
      Area       : Vis.Absolute.Rectangle_2d;
   begin
      --  Clear has happened before this is called.
      --  Correct Buffer is set as background pixmap therefore
      --  nothing needs to be drawn except if graph widget was polluted

      --  Relevant Fields in Event: Area, Count, Graphics_Expose
      --  type: Expose
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "expose_event");
      --  Ignore event, if more expose events are to follow.
      if Glib.">" (Gdk.Event.Get_Count (Event), 0) then
         return True;
      end if;

      if States.Has_Display_Changed (Widget) or else
        States.Has_Temporary_Changed (Widget) then

         Gdk_Area := Gdk.Event.Get_Area (Event);
         Area := Vis.Absolute.Combine_Rectangle
           (X_1 => Vis.Absolute_Int (Gdk_Area.X),
            Y_1 => Vis.Absolute_Int (Gdk_Area.Y),
            X_2 => Vis.Absolute_Int (Gdk_Area.X) +
                     Vis.Absolute_Int (Gdk_Area.Width) - 1,
            Y_2 => Vis.Absolute_Int (Gdk_Area.Y) +
                     Vis.Absolute_Int (Gdk_Area.Height) - 1);

         Drawing.Update_Display (Widget, Area);

         if States.Has_Temporary_Changed (Widget) then
            Drawing.Update_Temporary (Widget, Area);
         end if;
         Gdk.Window.Clear_Area
           (Window => Get_Window (Widget),
            X      => Glib.Gint (Vis.Absolute.Get_Left (Area)),
            Y      => Glib.Gint (Vis.Absolute.Get_Top (Area)),
            Width  => Glib.Gint (Vis.Absolute.Get_Width (Area)),
            Height => Glib.Gint (Vis.Absolute.Get_Height (Area)));
         States.Updated_Visual (Widget);
         States.Updated_Temporary (Widget);
      end if;

      return True;
   end On_Expose_Event;

   function On_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is

      Edge                    : Vis_Data.Vis_Edge_Id;
      Node                    : Vis_Data.Vis_Node_Id;
      Relative_Click_Position : Vis.Absolute.Vector_2d;
      Click_Position          : Vis.Absolute.Vector_2d;
      Window_Origin           : Vis.Absolute.Vector_2d :=
        Vis.Absolute.Get_Top_Left (Drawing.Get_Visible_Area (Widget));
      Value                   : Boolean;
   begin
      if States.Is_Drag_Current (Widget) then
         Cancel_Drag (Widget);
         return True;
      elsif States.Is_Rectangle_Current (Widget) then
         --  Cancel_Rectangle (Widget);
         return True;
      elsif States.Is_Click_Current (Widget) then
         Cancel_Click (Widget);
         return True;
      end if;

      Relative_Click_Position := Vis.Absolute.Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
      Click_Position := Vis.Absolute."+"
        (Window_Origin, Relative_Click_Position);

      if (Glib."=" (Gdk.Event.Get_Button (Event), Left_Button)) then
         --  left button
         if States.Is_Action_Mode_Current (Widget) then
            --  action mode, signal event
            Notifications.Action_Mode_Button_Press_Event
              (Widget   => Widget,
               Event    => Event,
               Location => Positioning.Get_Logic (Widget, Click_Position));
         else
            --  must handle click
            Value := Gdk.Main.Pointer_Grab
              (Window       => Get_Window (Widget),
               Owner_Events => False,
               Event_Mask   => Handled_Event_Mask,
               Confine_To   => Gdk.Window.Null_Window,  --  do not capture
               Cursor       => Gdk.Cursor.Null_Cursor,
               Time         => Gdk.Event.Get_Time (Event));
            --  Meaning of 'Value' is not documented in GtkAda! Therefore
            --  we ignore it here.

            Node := Vis_Data.Get_Node_At (Widget.Manager, Click_Position);
            if Vis_Data."/=" (Node, null) then
               --  pressed on node
               States.Begin_Click_On_Node
                 (Widget => Widget,
                  Point  => Click_Position,
                  Node   => Node);
            else
               Edge := Vis_Data.Get_Edge_At (Widget.Manager, Click_Position);
               if Vis_Data."/=" (Edge, null) then
                  --  pressed on edge
                  States.Begin_Click_On_Edge
                    (Widget => Widget,
                     Point  => Click_Position,
                     Edge   => Edge);
               else
                  --  pressed on background
                  States.Begin_Click_On_Background
                    (Widget => Widget,
                     Point  => Click_Position);
               end if;
            end if;
         end if;
         return True;
      elsif (Glib."=" (Gdk.Event.Get_Button (Event), Right_Button)) then
         --  right button
         Node := Vis_Data.Get_Node_At (Widget.Manager, Click_Position);
         if Vis_Data."/=" (Node, null) then
            Notifications.Node_Popup (Widget, Event, Node);
         else
            Edge := Vis_Data.Get_Edge_At (Widget.Manager, Click_Position);
            if Vis_Data."/=" (Edge, null) then
               Notifications.Edge_Popup (Widget, Event, Edge);
            else
               Notifications.Background_Popup
                 (Widget   => Widget,
                  Event    => Event,
                  Location => Positioning.Get_Logic (Widget, Click_Position));
            end if;
         end if;
         return True;
      elsif (Glib."=" (Gdk.Event.Get_Button (Event), Middle_Button)) then
         --  middle button
         return True;
      else
         return False;
      end if;
   end On_Button_Press_Event;

   function On_Button_Release_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is

      Modifiers : Gdk.Types.Gdk_Modifier_Type := Gdk.Event.Get_State (Event);
      Mode      : Selection_Modify_Type;
   begin
      if (Glib."=" (Gdk.Event.Get_Button (Event), Left_Button)) then
         Gdk.Main.Pointer_Ungrab (Gdk.Event.Get_Time (Event));

         if States.Is_Click_Current (Widget) then
            --  simple click
            Mode := Interpret_Modifiers (Modifiers);

            Process_Mouse_Click (Widget, Mode);

            States.End_Click (Widget);
         elsif States.Is_Rectangle_Current (Widget) then
            --  user has finished opening a rectangle
            States.End_Rectangle (Widget);
            States.Changed_Temporary (Widget);
            Redraw (Widget);
         elsif States.Is_Drag_Current (Widget) then
            Drop (Widget);
         end if;

         return True;
      else
         return False;
      end if;
   end On_Button_Release_Event;

   function On_Motion_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean is

      use Vis.Absolute;
      Distance                 : Vis.Absolute.Vector_2d;
      Relative_Motion_Position : Vis.Absolute.Vector_2d;
      Motion_Position          : Vis.Absolute.Vector_2d;
      Window_Origin            : Vis.Absolute.Vector_2d :=
        Vis.Absolute.Get_Top_Left (Drawing.Get_Visible_Area (Widget));
   begin
      Relative_Motion_Position := Vis.Absolute.Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
      Motion_Position := Vis.Absolute."+"
        (Window_Origin, Relative_Motion_Position);

      States.Set_Mouse_Position
        (Widget => Widget,
         Point  => Motion_Position);

      if States.Is_Click_Current (Widget) then
         Distance := States.Get_Mouse_Move_Distance (Widget);
         if Distance * Distance > Default_Click_Distance_Tolerance**2 then
            Begin_Mouse_Move_Action
              (Widget          => Widget,
               Mode            => Interpret_Modifiers
                                    (Gdk.Event.Get_State (Event)));
         end if;

         return True;
      elsif States.Is_Rectangle_Current (Widget) then
         States.Changed_Temporary (Widget);
         return True;
      elsif States.Is_Drag_Current (Widget) then
         States.Changed_Temporary (Widget);
         Redraw (Widget);
         return True;
      else
         return False;
      end if;
   end On_Motion_Notify_Event;

end Giant.Graph_Widgets.Callbacks;
