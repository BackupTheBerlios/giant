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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-graph_window-callbacks.ads,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003/09/08 15:33:10 $
--
------------------------------------------------------------------------------
--
--  Provides callbacks for graph window.
--

with Gtk.Arguments;
with Gtk.Widget;

with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Menu_Factory;

package Giant.Graph_Window.Callbacks is

   ---------------------------------------------------------------------------
   --  Background Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Background_Create_Pin
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Make_Room
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Background_Select_All
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Select_Nothing
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Edge Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edge_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Edge_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edge_Show_Target
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edge_Zoom
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Graph Widget Callbacks
   ---------------------------------------------------------------------------

   procedure On_Action_Mode_Button_Pressed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Background_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Edge_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Node_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Selection_Changed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Selection_Change_Action);

   ---------------------------------------------------------------------------
   --  Node Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Node_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Node_Show_Info
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Node_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Node_Annotate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Selection Callbacks
   ---------------------------------------------------------------------------

   procedure On_Apply_Layout
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Selection_List_Set_Operation
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Selection_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Selection_Zoom_To
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

end Giant.Graph_Window.Callbacks;
