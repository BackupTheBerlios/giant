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
--  $RCSfile: giant-graph_widgets-drawing.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/29 14:00:49 $
--
------------------------------------------------------------------------------


with Gdk.Window;
with Glib;

package body Giant.Graph_Widgets.Drawing is

   use Vis.Absolute;
   use type Vis.Absolute_Int;

   procedure Calculate_Display_Size
     (Widget        : access Graph_Widget_Record'Class;
      Result_Width  :    out Glib.Gint;
      Result_Height :    out Glib.Gint) is

      X             : Glib.Gint;
      Y             : Glib.Gint;
      Width         : Glib.Gint;
      Height        : Glib.Gint;
      Depth         : Glib.Gint;
      Window_Width  : Glib.Gint;
      Window_Height : Glib.Gint;
      Area          : Vis.Absolute.Rectangle_2d;
   begin
      Gdk.Window.Get_Geometry
        (Gdk.Window.Null_Window, X, Y, Width, Height, Depth);
      Gdk.Window.Get_Size
        (Get_Window (Widget), Window_Width, Window_Height);
      Width  := Glib.Gint'Max (Width, Window_Width);
      Height := Glib.Gint'Max (Height, Window_Height);
      Area := Vis.Absolute.Combine_Rectangle
        (X_1 => 0,
         Y_1 => 0,
         X_2 => Vis.Absolute_Int (Width) - 1,
         Y_2 => Vis.Absolute_Int (Height) - 1);
      Vis_Data.Optimize_Drawing_Area
        (Manager => Widget.Manager,
         Area    => Area);
      Result_Width := Glib.Gint (Get_Width (Area));
      Result_Height := Glib.Gint (Get_Height (Area));
   end Calculate_Display_Size;

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Height : Glib.Gint;
      Width  : Glib.Gint;
   begin
      Calculate_Display_Size (Widget, Width, Height);
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Buffer,
         Window => Window,
         Width  => Width,
         Height => Height);
      Gdk.Bitmap.Gdk_New
        (Bitmap => Widget.Drawing.Clip_Mask,
         Window => Window,
         Width  => Width,
         Height => Height);
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Display,
         Window => Window,
         Width  => Width,
         Height => Height);

--        Widget.Drawing.Background :=;
--        Widget.Drawing.Node_Border :=;
--        Widget.Drawing.Node_Fill :=;
--        Widget.Drawing.Node_Text :=;
--        Widget.Drawing.Light :=;

--        Widget.Drawing.Edge_Line :=;
--        Widget.Drawing.Edge_Label :=;
--        Widget.Drawing.Edge_Light :=;

--        Widget.Drawing.Font :=;
   end Set_Up;

end Giant.Graph_Widgets.Drawing;
