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
--  $RCSfile: giant-mini_maps.ads,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/07 12:47:19 $
--
------------------------------------------------------------------------------
--
--  This package contains the mini map used to display the current position
--  and size of the visual area within a graph widget.
--


with Gdk.Color;
with Gdk.GC;
with Gdk.Pixmap;
with Gtk.Widget;

pragma Elaborate_All (Gtk.Widget);

--  with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Mini_Maps is

   package Graph_Widgets is
      type Graph_Widget_Record is new Gtk.Widget.Gtk_Widget_Record
        with private;
      type Graph_Widget is access all Graph_Widget_Record;
      function Get_Logical_Area
        (Widget : access Graph_Widget_Record'Class)
        return Vis.Logic.Rectangle_2d;
      function Get_Visible_Area
        (Widget : access Graph_Widget_Record'Class)
        return Vis.Logic.Rectangle_2d;
   private
      type Graph_Widget_Record is new Gtk.Widget.Gtk_Widget_Record
        with null record;
   end Graph_Widgets;

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Mini_Map is access all Mini_Map_Record'Class;


   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null);

   procedure Initialize
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

   procedure Set_Graph_Widget
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

private

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record);

   Default_Width  : constant := 100;
   Default_Height : constant :=  70;

   --  must start at index 0
   type Mini_Map_Colors is (Black, Gray, Red, White);

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with
      record
         Watched           : Graph_Widgets.Graph_Widget := null;
         Transformation    : Vis.Transformation_Type;
         Buffer            : Gdk.Pixmap.Gdk_Pixmap  := Gdk.Pixmap.Null_Pixmap;
         Polluted          : Boolean                := True;
         Background_Gc     : Gdk.GC.Gdk_GC          := Gdk.GC.Null_GC;
         Logical_Area_Gc   : Gdk.GC.Gdk_GC          := Gdk.GC.Null_GC;
         Visible_Border_Gc : Gdk.GC.Gdk_GC          := Gdk.GC.Null_GC;
         Visible_Fill_Gc   : Gdk.GC.Gdk_GC          := Gdk.GC.Null_GC;
         Colors            : Gdk.Color.Gdk_Color_Array
           (Mini_Map_Colors'Pos (Mini_Map_Colors'First) ..
            Mini_Map_Colors'Pos (Mini_Map_Colors'Last));
      end record;

end Giant.Mini_Maps;
