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
--  $RCSfile: giant-graph_widgets-callbacks.adb,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/26 20:20:24 $
--
------------------------------------------------------------------------------


with Gdk.Types;
with Gtk.Arguments;
with Gtk.Handlers;

package body Giant.Graph_Widgets.Callbacks is

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


   package Realize_Handling is new Gtk.Widget.Realize_Handling
     (Widget_Type  => Graph_Widget_Record,
      Realize_Proc => On_Realize);


   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class) is

      use Gdk.Types;
   begin
      Set_Events
        (Widget,
         Get_Events (Widget) or Gdk.Types.Button_Press_Mask);

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

      Realize_Handling.Set_Realize (Widget);
   end Connect_All_Callbacks;

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Realize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end On_Realize;

   procedure After_Realize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end After_Realize;

   procedure On_Unrealize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end On_Unrealize;

   procedure On_Destroy
     (Widget : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end On_Destroy;

   procedure On_Size_Request
     (Widget      : access Graph_Widget_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access) is
   begin
      raise Unimplemented;
   end On_Size_Request;

   procedure On_Size_Allocate
     (Widget     : access Graph_Widget_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access) is
   begin
      raise Unimplemented;
   end On_Size_Allocate;

   function On_Expose_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end On_Expose_Event;

   function On_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end On_Button_Press_Event;

end Giant.Graph_Widgets.Callbacks;
