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
--  $RCSfile: giant-mini_maps.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--
--  This package contains the mini map used to display the current position
--  and size of the visual area within a graph widget.
--


with Gtk.Widget;

with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Mini_Maps is

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Mini_Map is access all Mini_Map_Record'Class;


   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null);

   procedure Set_Graph_Widget
     (Watched : in     Graph_Widgets.Graph_Widget);

private

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with
      record
         Watched      : Graph_Widgets.Graph_Widget;
         Logical_Area : Vis.Logic.Rectangle_2d
         Visible_Area : Vis.Logic.Rectangle_2d;
      end record;

end Giant.Mini_Maps;
