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
--  $RCSfile: giant-mini_maps.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/07 12:47:19 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;
with System;

with Gdk;
with Gdk.Drawable;
with Gdk.Event;
with Gdk.Rectangle;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Window;
--
with Gdk.Window_Attr;
with Gdk.Types;
with Gtk.Enums;
with Gtk.Style;
--
with Glib;
with Gtk.Arguments;
with Gtk.Handlers;
with Gtk.Object;
with Gtkada.Types;

with Giant.Logger;

package body Giant.Mini_Maps is

   use Vis.Absolute;
   use Vis.Logic;

   package body Graph_Widgets is
      function Get_Logical_Area
        (Widget : access Graph_Widget_Record'Class)
        return Vis.Logic.Rectangle_2d is
      begin
         return Combine_Rectangle (-5.0, 2.0, 5_000.0, 3_000.0);
      end Get_Logical_Area;

      function Get_Visible_Area
        (Widget : access Graph_Widget_Record'Class)
        return Vis.Logic.Rectangle_2d is
      begin
         return Combine_Rectangle (-0.2, 15.0, 400.0, 350.3);
      end Get_Visible_Area;
   end Graph_Widgets;

   package Mini_Map_Logger is new Logger
     (Name => "Giant.Mini_Map");


   ----------------------
   -- Reusable drawing --
   ----------------------

   procedure Draw_Rectangle
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Filled    : in     Boolean;
      Rectangle : in     Vis.Absolute.Rectangle_2d) is
   begin
      if Filled then
         Gdk.Drawable.Draw_Rectangle
           (Drawable => Drawable,
            Gc       => Gc,
            Filled   => True,
            X        => Glib.Gint (Get_Left (Rectangle)),
            Y        => Glib.Gint (Get_Top (Rectangle)),
            Width    => Glib.Gint (Get_Width (Rectangle)),
            Height   => Glib.Gint (Get_Height (Rectangle)));
      else
         Gdk.Drawable.Draw_Rectangle
           (Drawable => Drawable,
            Gc       => Gc,
            Filled   => False,
            X        => Glib.Gint (Get_Left (Rectangle)),
            Y        => Glib.Gint (Get_Top (Rectangle)),
            Width    => Glib.Gint (Get_Width (Rectangle) - 1),
            Height   => Glib.Gint (Get_Height (Rectangle) - 1));
      end if;
   end Draw_Rectangle;


   -------------------------
   -- Internal procedures --
   -------------------------

   procedure Resize_Buffer
     (Widget : access Mini_Map_Record'Class) is

      Window : Gdk.Window.Gdk_Window;
      Width  : Glib.Gint;
      Height : Glib.Gint;
   begin
      Window := Get_Window (Widget);
      Gdk.Window.Get_Size
        (Window,
         Width,
         Height);
      if Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Buffer);
      end if;
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Buffer,
         Window => Window,
         Width  => Width,
         Height => Height);
   end Resize_Buffer;

   procedure Destroy_Buffer
     (Widget : access Mini_Map_Record'Class) is
   begin
      if Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Buffer);
         Widget.Buffer := Gdk.Pixmap.Null_Pixmap;
      end if;
   end Destroy_Buffer;

   procedure Update
     (Widget : access Mini_Map_Record) is
   begin
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Widget.Polluted := True;
         Queue_Draw (Widget);
      end if;
   end Update;


   --------------------
   -- Implementation --
   --------------------

   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null) is
   begin
      Widget := new Mini_Map_Record;
      Initialize (Widget, Watched);
   end Create;

   procedure Set_Graph_Widget
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget) is
   begin
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Graph_Widgets.Unref (Widget.Watched);
      end if;
      Widget.Watched := Watched;
      Widget.Polluted := True;
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Graph_Widgets.Ref (Widget.Watched);
         Graph_Widgets.Sink (Widget.Watched);
      end if;
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Queue_Draw (Widget);
      end if;
   end Set_Graph_Widget;

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record) is

      procedure Clear
        (Area : Vis.Absolute.Rectangle_2d) is
      begin
         Draw_Rectangle
           (Drawable  => Widget.Buffer,
            Gc        => Widget.Background_Gc,
            Filled    => True,
            Rectangle => Area);
      end Clear;

      Width             : Glib.Gint;
      Height            : Glib.Gint;
      Window_Rect       : Vis.Absolute.Rectangle_2d;
      Target_Rect       : Vis.Absolute.Rectangle_2d;
      Visible_Rect      : Vis.Absolute.Rectangle_2d;
      Visible_Fill_Rect : Vis.Absolute.Rectangle_2d;
      Logical_Area      : Vis.Logic.Rectangle_2d;
      Visible_Area      : Vis.Logic.Rectangle_2d;
   begin
      Gdk.Window.Get_Size
        (Widget.Buffer,
         Width,
         Height);

      Window_Rect := Combine_Rectangle
        (0, 0, Vis.Absolute_Int (Width) - 1, Vis.Absolute_Int (Height) - 1);

      if Graph_Widgets."=" (Widget.Watched, null) then
         Clear (Window_Rect);
         return;
      end if;

      Logical_Area := Graph_Widgets.Get_Logical_Area (Widget.Watched);
      Visible_Area := Graph_Widgets.Get_Visible_Area (Widget.Watched);

      Widget.Transformation := Vis.Get_Transformation_Rect_Into_Rect_Centered
        (Logical_Area, Window_Rect);

      Target_Rect := Vis.Transform
        (Widget.Transformation, Logical_Area);

      if Get_Top (Window_Rect) < Get_Top (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Get_Top_Left (Window_Rect),
               Bottom_Right => Get_Top_Right (Target_Rect)
                                 - Combine_Vector (0, 1)));
      end if;
      if Get_Left (Window_Rect) < Get_Left (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Combine_Vector
                                 (Get_Left (Window_Rect),
                                  Get_Top  (Target_Rect)),
               Bottom_Right => Combine_Vector
                                 (Get_Left (Target_Rect) - 1,
                                  Get_Bottom (Target_Rect))));
      end if;
      if Get_Right (Window_Rect) > Get_Right (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Combine_Vector
                                 (Get_Right (Target_Rect) + 1,
                                  Get_Top (Target_Rect)),
               Bottom_Right => Combine_Vector
                                 (Get_Right (Window_Rect),
                                  Get_Bottom (Target_Rect))));
      end if;
      if Get_Bottom (Window_Rect) > Get_Bottom (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Get_Bottom_Left (Target_Rect)
                                 + Combine_Vector (0, 1),
               Bottom_Right => Get_Bottom_Right (Window_Rect)));
      end if;

      Draw_Rectangle
        (Drawable  => Widget.Buffer,
         Gc        => Widget.Logical_Area_Gc,
         Filled    => True,
         Rectangle => Target_Rect);

      Visible_Rect := Vis.Transform
        (Widget.Transformation, Visible_Area);

      Draw_Rectangle
        (Drawable  => Widget.Buffer,
         Gc        => Widget.Visible_Border_Gc,
         Filled    => False,
         Rectangle => Visible_Rect);

      if Get_Width (Visible_Rect) > 2
        and then Get_Height (Visible_Rect) > 2 then

         Visible_Fill_Rect := Combine_Rectangle
           (Get_Left (Visible_Rect) + 1,
            Get_Top (Visible_Rect) + 1,
            Get_Right (Visible_Rect) - 1,
            Get_Bottom (Visible_Rect) - 1);

         Draw_Rectangle
           (Drawable  => Widget.Buffer,
            Gc        => Widget.Visible_Fill_Gc,
            Filled    => True,
            Rectangle => Visible_Fill_Rect);
      end if;

   end Draw_Mini_Map;


   ---------------
   -- Callbacks --
   ---------------

   procedure On_Realize
     (Widget : access Mini_Map_Record'Class) is

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

      Set_User_Data
        (Window,
         Gtk.Get_Object (Widget));

      Gtk.Style.Set_Background
        (Get_Style (Widget),
         Get_Window (Widget),
         Gtk.Enums.State_Active);
   end On_Realize;

   procedure After_Realize
     (Widget : access Mini_Map_Record'Class) is

      Window           : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Colormap         : Gdk.Color.Gdk_Colormap := Get_Colormap (Widget);
      Number_Of_Colors : Glib.Gint;
      Success          : Glib.Boolean_Array
        (Widget.Colors'First .. Widget.Colors'Last);
   begin
      for I in Widget.Colors'Range loop
         declare
            Index      : Natural := I;
            Color_Name : String := Mini_Map_Colors'Image
              (Mini_Map_Colors'Val (I));
         begin
            Widget.Colors (Index) := Gdk.Color.Parse (Color_Name);
         exception
            when Gdk.Color.Wrong_Color =>
               Mini_Map_Logger.Error
                 ("Could not parse color name " & Color_Name & ". Will use"
                  & " undefined color ('Null_Color') instead.");
               Widget.Colors (Index) := Gdk.Color.Null_Color;
         end;
      end loop;

      Gdk.Color.Alloc_Colors
        (Colormap => Colormap,
         Colors   => Widget.Colors,
         Success  => Success,
         Result   => Number_Of_Colors);

      if Integer (Number_Of_Colors) < Widget.Colors'Length then
         for I in Success'Range loop
            if not Success (I) then
               Mini_Map_Logger.Error
                 ("Color " & Mini_Map_Colors'Image (Mini_Map_Colors'Val (I))
                  & " could not be allocated. Will use undefined color "
                  & "('Null_Color') instead");
               Widget.Colors (I) := Gdk.Color.Null_Color;
            end if;
         end loop;
      end if;

      Gdk.GC.Gdk_New (Widget.Background_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Background_Gc, Widget.Colors (Mini_Map_Colors'Pos (Black)));
      Gdk.GC.Gdk_New (Widget.Logical_Area_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Logical_Area_Gc, Widget.Colors (Mini_Map_Colors'Pos (Gray)));
      Gdk.GC.Gdk_New (Widget.Visible_Border_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Visible_Border_Gc, Widget.Colors (Mini_Map_Colors'Pos (Red)));
      Gdk.GC.Gdk_New (Widget.Visible_Fill_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Visible_Fill_Gc, Widget.Colors (Mini_Map_Colors'Pos (White)));

      Resize_Buffer (Widget);
   end After_Realize;

   procedure On_Unrealize
     (Widget: access Mini_Map_Record'Class) is
   begin
      Gdk.GC.Destroy (Widget.Background_Gc);
      Gdk.GC.Destroy (Widget.Logical_Area_Gc);
      Gdk.GC.Destroy (Widget.Visible_Border_Gc);
      Gdk.GC.Destroy (Widget.Visible_Fill_Gc);
      Gdk.Color.Free_Colors (Get_Colormap (Widget), Widget.Colors);
   end On_Unrealize;

   procedure On_Size_Request
     (Widget      : access Mini_Map_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access) is
   begin
      Requisition.Width  := Default_Width;
      Requisition.Height := Default_Height;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end On_Size_Request;

   procedure On_Size_Allocate
     (Widget     : access Mini_Map_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access) is
   begin
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Gdk.Window.Move_Resize
           (Get_Window (Widget),
            Allocation.X,
            Allocation.Y,
            Glib.Gint (Allocation.Width),
            Glib.Gint (Allocation.Height));

         Resize_Buffer (Widget);
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
   end On_Size_Allocate;

   function On_Expose_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean is

      Area : Gdk.Rectangle.Gdk_Rectangle;
   begin
      Mini_Map_Logger.Debug ("On_Expose_Event");
      --  Relevant Fields in Event: Area, Count, Graphics_Expose
      --  type: Expose
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "expose_event");
      --  Ignore event, if more expose events are to follow.
      if Glib.">" (Gdk.Event.Get_Count (Event), 0) then
         return True;
      end if;

      pragma Assert (Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap));

      if Widget.Polluted then
         Draw_Mini_Map (Widget);
      end if;

      Area := Gdk.Event.Get_Area (Event);
      Gdk.Drawable.Draw_Pixmap
        (Drawable => Get_Window (Widget),
         Gc       => Widget.Background_Gc,
         Src      => Widget.Buffer,
         Xsrc     => Area.X,
         Ysrc     => Area.Y,
         Xdest    => Area.X,
         Ydest    => Area.Y,
         Width    => Glib.Gint (Area.Width),
         Height   => Glib.Gint (Area.Height));

      return True;
   end On_Expose_Event;


   ----------------------------------
   -- Initialization & destruction --
   ----------------------------------

   package Mini_Map_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Mini_Map_Record);
   package Mini_Map_Boolean_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Mini_Map_Record,
      Return_Type => Boolean);

   package Realize_Cbs renames Mini_Map_Callback;

   package Unrealize_Cbs renames Mini_Map_Callback;

   package Size_Request_Cbs renames Mini_Map_Callback;
   package Requisition_Marshallers is new
     Size_Request_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Requisition_Access,
        Conversion => Gtk.Arguments.To_Requisition);

   package Size_Allocate_Cbs renames Size_Request_Cbs;
   package Allocation_Marshallers is new
     Size_Allocate_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Allocation_Access,
        Conversion => Gtk.Arguments.To_Allocation);

   package Expose_Cbs renames Mini_Map_Boolean_Callback;


   package Realize_Handling is new Gtk.Widget.Realize_Handling
     (Widget_Type  => Mini_Map_Record,
      Realize_Proc => On_Realize);


   Class_Record : System.Address := System.Null_Address;

   procedure Initialize
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget) is
   begin
      Gtk.Widget.Initialize_Widget (Widget);
      Widget.Watched := null;
      Widget.Buffer := Gdk.Pixmap.Null_Pixmap;
      Widget.Polluted := True;
      Set_Graph_Widget (Widget, Watched);
      Gtk.Object.Initialize_Class_Record
        (Object       => Widget,
         Signals      => Gtkada.Types.Null_Array,
         Class_Record => Class_Record);

      Realize_Cbs.Connect
        (Widget => Widget,
         Name   => "realize",
         Marsh  => Realize_Cbs.To_Marshaller (After_Realize'Access),
         After  => True);
      Unrealize_Cbs.Connect
        (Widget => Widget,
         Name   => "unrealize",
         Marsh  => Unrealize_Cbs.To_Marshaller (On_Unrealize'Access));
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
      Expose_Cbs.Connect
        (Widget => Widget,
         Name   => "expose_event",
         Marsh  => Expose_Cbs.To_Marshaller (On_Expose_Event'Access));

      Realize_Handling.Set_Realize (Widget);
   end Initialize;

end Giant.Mini_Maps;

