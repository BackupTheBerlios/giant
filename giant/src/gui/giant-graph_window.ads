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
--  $RCSfile: giant-graph_window.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/03 19:20:59 $
--
------------------------------------------------------------------------------
--
-- Provides a progress dialog.
--
-- Emits the "cancelled" callback when the cancel button is pressed.
--

with Gtk.Label;

with Gtk.Clist;
with Gtk.Combo;
with Gtk.Gentry;
with Gtk.Menu;
with Gtk.Option_Menu;
with Gtk.Paned;
with Gtk.Window;

package Giant.Graph_Window is

   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with private;

   type Graph_Window_Access is access all Graph_Window_Record'Class;

   procedure Create
     (Window : out Graph_Window_Access);

   procedure Initialize
     (Window : access Graph_Window_Record'Class);

private
   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with record
		Split_Pane : Gtk.Paned.Gtk_Hpaned;
		Pin_List : Gtk.Clist.Gtk_Clist;
		Pin_Popup_Menu : Gtk.Menu.Gtk_Menu;
		Selection_List : Gtk.Clist.Gtk_Clist;
		Selection_Popup_Menu : Gtk.Menu.Gtk_Menu;
		Vis_Style_Menu : Gtk.Option_Menu.Gtk_Option_Menu;
		Zoom_Combo : Gtk.Combo.Gtk_Combo;
		Zoom_Entry : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Graph_Window;
