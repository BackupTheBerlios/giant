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
--  $RCSfile: giant-graph_widgets-callbacks.ads,v $, $Revision: 1.3 $
--  $Author: keulsn $
--  $Date: 2003/07/02 16:49:15 $
--
------------------------------------------------------------------------------


with Gdk.Event;
with Gtk.Adjustment;

package Giant.Graph_Widgets.Callbacks is

   ----------------------------------------------------------------------------
   --  Connects all Callbacks 'Widget' needs
   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class);

private

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

end Giant.Graph_Widgets.Callbacks;
