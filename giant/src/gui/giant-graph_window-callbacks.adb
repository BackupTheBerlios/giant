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
--  $RCSfile: giant-graph_window-callbacks.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/07/10 16:26:35 $
--

with Giant.Layout_Dialog;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;

package body Giant.Graph_Window.Callbacks is

   ---------------------------------------------------------------------------
   --  Background Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Background_Make_Room
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   procedure On_Background_Create_Pin
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   ---------------------------------------------------------------------------
   --  Edge Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edge_Zoom
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   ---------------------------------------------------------------------------
   --  Graph Widget Callbacks
   ---------------------------------------------------------------------------

   procedure On_Action_Mode_Button_Pressed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      use type Actions.Graph_Window_Action_Access;

      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      if (Gui_Manager.Actions.Is_Action_Pending) then
         Gui_Manager.Actions.Trigger (Window, null, Vis.Logic.Zero_2d);
      elsif (Window.Local_Action /= null) then
         Actions.Execute
           (Window.Local_Action, Window, null, Vis.Logic.Zero_2d);
      end if;
   end On_Action_Mode_Button_Pressed;

   procedure On_Edge_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Edge_Popup_Action)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Gtk.Menu.Show_All (Window.Edge_Menu);
      Gtk.Menu.Popup (Window.Edge_Menu,
                      Button => Gdk.Event.Get_Button (Event.Event),
                      Activate_Time => Gdk.Event.Get_Time (Event.Event));
   end On_Edge_Popup;

   procedure On_Node_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Node_Popup_Action)
   is
   begin
      null;
   end On_Node_Popup;

   procedure On_Selection_Changed
     (Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Action     : in     Graph_Widgets.Notifications.Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection)
   is
   begin
      null;
   end On_Selection_Changed;

   ---------------------------------------------------------------------------
   --  Node Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Node_Show_Info
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   procedure On_Node_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   procedure On_Node_Annotate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end;

   ---------------------------------------------------------------------------
   --  Selection Callbacks
   ---------------------------------------------------------------------------

   procedure On_Apply_Layout
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Layout_Dialog.Show (Get_Window_Name (Window),
                          Get_Selected_Selection (Window));
   end On_Apply_Layout;


end Giant.Graph_Window.Callbacks;
