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
--  $RCSfile: giant-mini_maps.ads,v $, $Revision: 1.3 $
--  $Author: keulsn $
--  $Date: 2003/06/09 01:13:39 $
--
------------------------------------------------------------------------------
--
--  This package contains the mini map used to display the current position
--  and size of the visual area within a graph widget.
--


with Gdk.Color;
with Gdk.GC;
with Gdk.Pixmap;
with Gtk.Handlers;
with Gtk.Widget;

pragma Elaborate_All (Gtk.Widget);

--  with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Mini_Maps is

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Mini_Map is access all Mini_Map_Record'Class;


   --  ugly test code because Graph_Lib not done yet
   package Graph_Widgets is
      type Graph_Widget_Record is new Gtk.Widget.Gtk_Widget_Record
        with private;
      type Graph_Widget is access all Graph_Widget_Record;
      function Create return Graph_Widget;
      procedure Set_Location
        (Widget     : access Graph_Widget_Record'Class;
         Location   : in     Vis.Logic.Vector_2d);
      procedure Set_User_Data
        (Data   : in     Mini_Map);
      procedure Set_Logical_Area
        (Widget : access Graph_Widget_Record'Class;
         Area   : in     Vis.Logic.Rectangle_2d);
      procedure Set_Visible_Area
        (Widget : access Graph_Widget_Record'Class;
         Area   : in     Vis.Logic.Rectangle_2d);
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
   --  end of ugly test code


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

   procedure Update
     (Widget : access Mini_Map_Record);

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record);

   Default_Width  : constant := 100;
   Default_Height : constant :=  70;

   --  must start at index 0, Mini_Map_Colors'Images (E) must be
   --  recognized by Gdk.Color.Parse for every E : Mini_Map_Colors
   type Mini_Map_Colors is (Black, Red, White);

   type Mini_Map_Record is new Gtk.Widget.Gtk_Widget_Record with
      record
         Watched              : Graph_Widgets.Graph_Widget := null;
         Logical_Area_Handler : Gtk.Handlers.Handler_Id;
         Visible_Area_Handler : Gtk.Handlers.Handler_Id;
         Transformation       : Vis.Transformation_Type;
         Buffer               : Gdk.Pixmap.Gdk_Pixmap      :=
                                  Gdk.Pixmap.Null_Pixmap;
         Polluted             : Boolean                    := True;
         Background_Gc        : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Logical_Area_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Border_Gc    : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Fill_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Colors               : Gdk.Color.Gdk_Color_Array
           (Mini_Map_Colors'Pos (Mini_Map_Colors'First) ..
            Mini_Map_Colors'Pos (Mini_Map_Colors'Last));
      end record;

end Giant.Mini_Maps;
