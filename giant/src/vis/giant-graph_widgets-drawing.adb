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
--  $RCSfile: giant-graph_widgets-drawing.adb,v $, $Revision: 1.9 $
--  $Author: keulsn $
--  $Date: 2003/07/09 19:45:36 $
--
------------------------------------------------------------------------------


with Gdk.Types;
with Gdk.Drawable;
with Gdk.Window;
with Glib;

with Giant.Graph_Lib.Node_Attribute_Filters;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;

package body Giant.Graph_Widgets.Drawing is


   use Vis.Absolute;


   package Drawing_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Drawing");


   type Layer_Clipping_Array is array (Positive range <>) of
     Vis_Data.Layer_Clipping_Access;


   function Get_Height
     (Font : in     Gdk.Font.Gdk_Font)
     return Vis.Absolute_Int is
   begin
      return Vis.Absolute_Int (Gdk.Font.Get_Ascent (Font)) +
        Vis.Absolute_Int (Gdk.Font.Get_Descent (Font));
   end Get_Height;

   function Get_Number_Of_Global_Lights
     (Node : Vis_Data.Vis_Node_Id)
     return Natural is

      Number_Of_Lights : Natural;
      Highlighting : Vis_Data.Flags_Type := Vis_Data.Get_Highlighting (Node);
   begin
      Number_Of_Lights := 0;
      for I in Vis_Data.Global_Highlight_Type loop
         if Highlighting (I) then
            Number_Of_Lights := Number_Of_Lights + 1;
         end if;
      end loop;
      return Number_Of_Lights;
   end Get_Number_Of_Global_Lights;

   ----------------------------------------------------------------------------
   --  Clears a drawable using Gc
   procedure Clear
     (Gc       : in     Gdk.GC.Gdk_GC;
      Drawable : in     Gdk.Drawable.Gdk_Drawable) is

      Width  : Glib.Gint;
      Height : Glib.Gint;
   begin
      Gdk.Window.Get_Size
        (Window   => Drawable,
         Width    => Width,
         Height   => Height);
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => True,
         X        => 0,
         Y        => 0,
         Width    => Width,
         Height   => Height);
   end Clear;

   procedure Draw_Filled
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Rectangle : in     Vis.Absolute.Rectangle_2d;
      Origin    : in     Vis.Absolute.Vector_2d) is

      Top_Left : Vis.Absolute.Vector_2d := Get_Top_Left (Rectangle) - Origin;
      X        : Glib.Gint              := Glib.Gint (Get_X (Top_Left));
      Y        : Glib.Gint              := Glib.Gint (Get_Y (Top_Left));
   begin
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => True,
         X        => X,
         Y        => Y,
         Width    => Glib.Gint (Get_Width (Rectangle)),
         Height   => Glib.Gint (Get_Height (Rectangle)));
   end Draw_Filled;
   pragma Inline (Draw_Filled);

   procedure Draw_Border
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Rectangle : in     Vis.Absolute.Rectangle_2d;
      Origin    : in     Vis.Absolute.Vector_2d) is

      Top_Left : Vis.Absolute.Vector_2d := Get_Top_Left (Rectangle) - Origin;
      X        : Glib.Gint              := Glib.Gint (Get_X (Top_Left));
      Y        : Glib.Gint              := Glib.Gint (Get_Y (Top_Left));
   begin
      Gdk.Drawable.Draw_Rectangle
        (Drawable => Drawable,
         Gc       => Gc,
         Filled   => False,
         X        => X,
         Y        => Y,
         Width    => Glib.Gint (Get_Width (Rectangle) - 1),
         Height   => Glib.Gint (Get_Height (Rectangle) - 1));
   end Draw_Border;
   pragma Inline (Draw_Border);

   procedure Draw_Text
     (Buffer : in     Gdk.Drawable.Gdk_Drawable;
      Font   : in     Gdk.Font.Gdk_Font;
      Gc     : in     Gdk.GC.Gdk_GC;
      Area   : in     Vis.Absolute.Rectangle_2d;
      Text   : in     String) is

      use type Glib.Gint;
      Width          : Glib.Gint := Glib.Gint (Get_Width (Area));
      Text_Width     : Glib.Gint;
      Avg_Char_Width : Glib.Gint;
      Minimum        : Integer;
      Maximum        : Integer;
      Last           : Integer;
   begin
      if Get_Height (Font) > Get_Height (Area) or else Text'Length < 1 then
         return;
      end if;

      Text_Width := Gdk.Font.Text_Measure (Font, Text);
      if Text_Width > Width then
         --  leave width for abbreviation string
         Width := Width -
           Gdk.Font.Text_Measure (Font, Default_Text_Abbreviation);

         --  can draw at least abbreviation string?
         if Width >= 0 then
            --  Text (Text'First .. Minimum) can be drawn.
            Minimum := Text'First - 1;
            --  upper bound for maximum number of characters
            Maximum := Text'Last - 1;
            --  Maximum >= Minimum because of Text'Length >= 1 (see above)

            --  last time tried this bound
            Last := Text'Last;
            --  Last >= Text'First because of Text'Length >= 1 (see above)

            while Maximum > Minimum loop
               --  Last >= Text'First ==> Last - Text'First + 1 > 0
               Avg_Char_Width := Text_Width /
                 Glib.Gint (Last - Text'First + 1);
               if Avg_Char_Width > 0 then
                  Last := Integer (Width / Avg_Char_Width) + Text'First;
               end if;
               if Last > Maximum then
                  Last := Maximum;
               elsif Last <= Minimum then
                  Last := Minimum + 1;
                  --  Text'First <= Minimum + 1 <= Last ==> Last >= Text'First
               end if;
               --  Minimum + 1 <= Last <= Maximum
               Text_Width := Gdk.Font.Text_Width
                 (Font, Text (Text'First .. Last));
               if Text_Width < Width then
                  --  Minimum becomes greater, Text (Text'First .. Minimum)
                  --  will still be ok.
                  Minimum := Last;
               elsif Text_Width > Width then
                  --  Maximum becomes smaller
                  Maximum := Last - 1;
               else
                  --  Minimum becomes greater (found optimum anyway)
                  Minimum := Last;
                  Maximum := Last;
               end if;
               --  loop will terminate because in each iteration either
               --  Minimum becomes greater or Maximum becomes smaller
               --  Minimum is assured to be a possible value
            end loop;

            Gdk.Drawable.Draw_Text
              (Drawable => Buffer,
               Font     => Font,
               Gc       => Gc,
               X        => Glib.Gint (Get_Left (Area)),
               Y        => Glib.Gint (Get_Top (Area)) +
                             Gdk.Font.Get_Ascent (Font),
               Text     => Text (Text'First .. Minimum) &
                             Default_Text_Abbreviation);

         else
            null;
            --  no drawing done at all: no space for entire text and no
            --  space for abbreviation.
         end if;
      else
         Gdk.Drawable.Draw_Text
           (Drawable => Buffer,
            Font     => Font,
            Gc       => Gc,
            X        => Glib.Gint (Get_Left (Area)),
            Y        => Glib.Gint (Get_Top (Area)) +
                          Gdk.Font.Get_Ascent (Font),
            Text     => Text);
      end if;
   end Draw_Text;

   procedure Apply_Clipping
     (Widget   : access Graph_Widget_Record'Class;
      Clipping : in     Vis_Data.Layer_Clipping_Type;
      Origin   : in     Vis.Absolute.Vector_2d) is
   begin
      case Clipping.Action is
         when Vis_Data.Add =>
            Draw_Filled
              (Drawable  => Widget.Drawing.Clip_Mask,
               Gc        => Widget.Drawing.Clip_Open,
               Rectangle => Clipping.Area,
               Origin    => Origin);
         when Vis_Data.Delete =>
            Draw_Filled
              (Drawable  => Widget.Drawing.Clip_Mask,
               Gc        => Widget.Drawing.Clip_Close,
               Rectangle => Clipping.Area,
               Origin    => Origin);
      end case;
   end Apply_Clipping;
   pragma Inline (Apply_Clipping);

   procedure Revert_Clipping
     (Widget   : access Graph_Widget_Record'Class;
      Clipping : in     Vis_Data.Layer_Clipping_Type;
      Origin   : in     Vis.Absolute.Vector_2d) is
   begin
      case Clipping.Action is
         when Vis_Data.Add =>
            Draw_Filled
              (Drawable  => Widget.Drawing.Clip_Mask,
               Gc        => Widget.Drawing.Clip_Close,
               Rectangle => Clipping.Area,
               Origin    => Origin);
         when Vis_Data.Delete =>
            null;
      end case;
   end Revert_Clipping;
   pragma Inline (Revert_Clipping);

   --  NOTE: Must be synced with 'Draw_Edge'
   procedure Update_Edge_Size
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Number_Of_Lights : Natural;
      Highlighting     : Vis_Data.Flags_Type :=
        Vis_Data.Get_Highlighting (Edge);
      Width            : Vis.Absolute_Natural;
      Height           : Vis.Absolute_Natural;
      Font             : Gdk.Font.Gdk_Font;
      Graph_Edge       : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Number_Of_Lights := 0;
      for I in Vis_Data.Highlight_Type loop
         if Highlighting (I) then
            Number_Of_Lights := Number_Of_Lights + 1;
         end if;
      end loop;
      Vis_Data.Set_Thickness
        (Edge,
         Default_Edge_Line_Thickness +
           Number_Of_Lights * Default_Edge_Light_Thickness);

      if Settings.Show_Edge_Label (Widget, Edge) then
         Font := Settings.Get_Edge_Font (Widget);
         Height := Settings.Get_Edge_Font_Height (Widget);
         Width := Vis.Absolute_Natural
           (Gdk.Font.Text_Measure (Font,
                                   Graph_Lib.Get_Edge_Tag (Graph_Edge))) + 1;
         Vis_Data.Set_Text_Area_Size (Edge, Combine_Vector (Width, Height));
      else
         Vis_Data.Remove_Text_Area (Edge);
      end if;
   end Update_Edge_Size;

   ----------------------------------------------------------------------------
   --  Draws an edge onto 'Buffer'
   --  NOTE: Must be synced with 'Update_Edge_Size'
   procedure Draw_Edge
     (Widget : access Graph_Widget_Record'Class;
      Buffer : in     Gdk.Pixmap.Gdk_Pixmap;
      Edge   : in     Vis_Data.Vis_Edge_Id;
      Origin : in     Vis.Absolute.Vector_2d) is

      procedure Draw_Edge_Line
        (Gc     : in     Gdk.GC.Gdk_GC;
         From   : in     Vis.Absolute.Vector_2d;
         To     : in     Vis.Absolute.Vector_2d) is

         From_X : Glib.Gint;
         From_Y : Glib.Gint;
         To_X   : Glib.Gint;
         To_Y   : Glib.Gint;
      begin
         Vis.To_Gdk (From, From_X, From_Y);
         Vis.To_Gdk (To, To_X, To_Y);
         Gdk.Drawable.Draw_Line
           (Drawable   => Buffer,
            Gc         => Gc,
            X1         => From_X,
            Y1         => From_Y,
            X2         => To_X,
            Y2         => To_Y);
      end Draw_Edge_Line;

      procedure Draw_All_Edge_Lines
        (Gc     : in     Gdk.GC.Gdk_GC;
         Style  : in     Edge_Style_Type;
         Width  : in     Vis.Absolute_Natural) is

         Source     : Vis.Absolute.Vector_2d;
         Target     : Vis.Absolute.Vector_2d;
         Line_Style : Gdk.Types.Gdk_Line_Style;
      begin
         case Style is
            when Continuous_Line =>
               Line_Style := Gdk.Types.Line_Solid;
            when Dashed_Line =>
               Line_Style := Gdk.Types.Line_On_Off_Dash;
            when Dotted_Line =>
               Line_Style := Gdk.Types.Line_On_Off_Dash;
         end case;
         --  set line width
         Gdk.GC.Set_Line_Attributes
           (GC         => Gc,
            Line_Width => Glib.Gint (Width),
            Line_Style => Line_Style,
            Cap_Style  => Gdk.Types.Cap_Round,
            Join_Style => Gdk.Types.Join_Round);
         --  Cannot use 'Gdk.Drawable.Draw_Lines' because
         --  'Gdk.Types.Gdk_Points_Array' uses Glib.Gint16 as component type
         --  wich is too small.
         Target := Vis_Data.Get_Point (Edge, 1) - Origin;
         for I in 2 .. Vis_Data.Get_Number_Of_Points (Edge) loop
            Source := Target;
            Target := Vis_Data.Get_Point (Edge, I) - Origin;
            Draw_Edge_Line
              (Gc     => Gc,
               From   => Source,
               To     => Target);
         end loop;
         --  Target contains end point
         Draw_Edge_Line
           (Gc     => Gc,
            From   => Vis_Data.Get_Left_Arrow_Point (Edge) - Origin,
            To     => Target);
         Draw_Edge_Line
           (Gc     => Gc,
            From   => Vis_Data.Get_Right_Arrow_Point (Edge) - Origin,
            To     => Target);
      end Draw_All_Edge_Lines;

      type Light_Array is array
        (1 .. Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'Last) -
              Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1)
        of Vis_Data.Local_Highlight_Type;
      Lights                : Light_Array;
      Light_Count           : Integer;
      Current_Thickness     : Vis.Absolute_Natural;
      Light_Extra_Thickness : Vis.Absolute_Natural;
      Style                 : Edge_Style_Type :=
        Settings.Get_Edge_Style (Widget, Edge);
      Highlighting          : Vis_Data.Flags_Type :=
        Vis_Data.Get_Highlighting (Edge);
   begin
      if Vis_Data.Is_Hidden (Edge) then
         return;
      end if;

      Light_Count := Lights'First - 1;
      for Light in Vis_Data.Highlight_Type loop
         if Highlighting (Light) then
            Light_Count := Light_Count + 1;
            Lights (Light_Count) := Light;
         end if;
      end loop;
      if Light_Count >= Lights'First then
         Current_Thickness := Vis_Data.Get_Thickness (Edge);
         Light_Extra_Thickness :=
           (Current_Thickness - Default_Edge_Line_Thickness) /
           (Light_Count - Lights'First + 1);
         loop
            Draw_All_Edge_Lines
              (Gc    => Widget.Drawing.Edge_Light (Lights (Light_Count)),
               Style => Continuous_Line,
               Width => Current_Thickness);

            Light_Count := Light_Count - 1;
            exit when Light_Count < Lights'First;
            Current_Thickness := Current_Thickness - Light_Extra_Thickness;
         end loop;
      end if;

      Draw_All_Edge_Lines
        (Gc    => Widget.Drawing.Edge_Line (Style),
         Style => Style,
         Width => Default_Edge_Line_Thickness);

      if Vis_Data.Has_Text_Area (Edge) then
         Draw_Text
           (Buffer => Buffer,
            Font   => Settings.Get_Edge_Font (Widget),
            Gc     => Widget.Drawing.Edge_Label,
            Area   => Vis_Data.Get_Text_Area (Edge),
            Text   => Graph_Lib.Get_Edge_Tag (Vis_Data.Get_Graph_Edge (Edge)));
      end if;
   end Draw_Edge;


   --  NOTE: Must be synced with 'Draw_Node'
   procedure Update_Node_Size
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Height               : Vis.Absolute_Natural;
      Width                : Vis.Absolute_Natural;
      Number_Of_Lights     : Natural := Get_Number_Of_Global_Lights (Node);
      Border_Thickness     : Vis.Absolute_Natural;
      Attributes_Height    : Vis.Absolute_Natural;
      Number_Of_Attributes : Natural :=
        Settings.Get_Node_Attribute_Count (Widget, Node);
      Font_Height          : Vis.Absolute_Natural :=
        Settings.Get_Node_Font_Height (Widget);
      Icon_Height          : Vis.Absolute_Natural;
      Annotation_Height    : Vis.Absolute_Natural;
      Header_Height        : Vis.Absolute_Natural;
      Highlighting         : Vis_Data.Flags_Type :=
        Vis_Data.Get_Highlighting (Node);
   begin
      Border_Thickness := Number_Of_Lights * Default_Node_Light_Thickness + 1;

      Attributes_Height := Number_Of_Attributes *
        (Default_Text_Spacing + Font_Height);

      Header_Height := Default_Text_Spacing + Font_Height;
      Icon_Height := Get_Y (Settings.Get_Node_Icon_Size (Widget, Node));
      if Icon_Height > Header_Height then
         Header_Height := Icon_Height;
      end if;
      if Vis_Data.Is_Annotated (Node) then
         Annotation_Height := Get_Y
           (Settings.Get_Annotation_Icon_Size (Widget));
         if Annotation_Height > Header_Height then
            Header_Height := Annotation_Height;
         end if;
      end if;

      Height := Border_Thickness + Header_Height;
      if Settings.Show_Node_Class_Name (Widget, Node) then
         Height := Height + Default_Text_Spacing + Font_Height;
      end if;
      Height := Height +
        Attributes_Height +
        Default_Text_Spacing +
        Border_Thickness;

      Width := 2 * Border_Thickness + Settings.Get_Node_Width (Widget);

      Vis_Data.Set_Node_Size (Node, Combine_Vector (Width, Height));
   end Update_Node_Size;

   --  NOTE: Must be synced with 'Draw_Node'
   function Get_Node_Border_Top_Center
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id)
     return Vis.Absolute.Vector_2d is
   begin
      return Vis_Data.Get_Top_Center (Node) + Combine_Vector
        (0, Get_Number_Of_Global_Lights (Node) * Default_Node_Light_Thickness);
   end Get_Node_Border_Top_Center;

   ----------------------------------------------------------------------------
   --  Draws a node onto 'Buffer'
   --  NOTE: Must be synced with 'Update_Node_Size' and
   --        'Get_Node_Border_Top_Center
   procedure Draw_Node
     (Widget : access Graph_Widget_Record'Class;
      Buffer : in     Gdk.Pixmap.Gdk_Pixmap;
      Node   : in     Vis_Data.Vis_Node_Id;
      Origin : in     Vis.Absolute.Vector_2d) is

      procedure Draw_Node_Highlighting
        (Gc        : in     Gdk.GC.Gdk_GC;
         Thickness : in     Vis.Absolute_Int;
         Rect      : in     Vis.Absolute.Rectangle_2d) is

         Top_Part    : Vis.Absolute.Rectangle_2d;
         Left_Part   : Vis.Absolute.Rectangle_2d;
         Right_Part  : Vis.Absolute.Rectangle_2d;
         Bottom_Part : Vis.Absolute.Rectangle_2d;
      begin
         pragma Assert (Thickness > 0);
         Top_Part := Combine_Rectangle
           (Top_Left     => Get_Top_Left (Rect),
            Bottom_Right => Combine_Vector
                              (X => Get_Right (Rect),
                               Y => Get_Top (Rect) + Thickness - 1));
         Bottom_Part := Combine_Rectangle
           (Top_Left     => Combine_Vector
                              (X => Get_Left (Rect),
                               Y => Get_Bottom (Rect) - Thickness + 1),
            Bottom_Right => Get_Bottom_Right (Rect));
         Left_Part := Combine_Rectangle
           (X_1 => Get_Left (Top_Part),
            Y_1 => Get_Bottom (Top_Part) + 1,
            X_2 => Get_Left (Top_Part) + Thickness - 1,
            Y_2 => Get_Top (Bottom_Part) - 1);
         Right_Part := Combine_Rectangle
           (X_1 => Get_Right (Top_Part) - Thickness + 1,
            Y_1 => Get_Bottom (Top_Part) + 1,
            X_2 => Get_Right (Top_Part),
            Y_2 => Get_Top (Bottom_Part) - 1);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Top_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Left_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Right_Part,
            Origin    => Zero_2d);
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Bottom_Part,
            Origin    => Zero_2d);
      end Draw_Node_Highlighting;

      procedure Draw_Node_Border
        (Rect      : in     Vis.Absolute.Rectangle_2d) is

         Gc : Gdk.GC.Gdk_GC := Widget.Drawing.Node_Border;
      begin
         Gdk.GC.Set_Foreground
           (GC        => Gc,
            Color     => Settings.Get_Node_Border_Color (Widget, Node));
         Draw_Border
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Rect,
            Origin    => Zero_2d);
      end Draw_Node_Border;

      procedure Draw_Node_Filling
        (Rect      : in     Vis.Absolute.Rectangle_2d) is

         Gc : Gdk.GC.Gdk_GC := Widget.Drawing.Node_Fill;
      begin
         Gdk.GC.Set_Foreground
           (GC        => Gc,
            Color     => Settings.Get_Node_Fill_Color (Widget, Node));
         Draw_Filled
           (Drawable  => Buffer,
            Gc        => Gc,
            Rectangle => Rect,
            Origin    => Zero_2d);
      end Draw_Node_Filling;

      procedure Draw_Node_Rectangle
        (Inner_Rect       :    out Vis.Absolute.Rectangle_2d) is

         type Light_Array is array
           (1 .. Vis_Data.Local_Highlight_Type'Pos
                   (Vis_Data.Local_Highlight_Type'Last) -
                 Vis_Data.Local_Highlight_Type'Pos
                   (Vis_Data.Local_Highlight_Type'First) + 1)
           of Vis_Data.Local_Highlight_Type;
         Lights          : Light_Array;
         Light_Count     : Integer;
         Light_Rect      : Vis.Absolute.Rectangle_2d;
         Light_Thickness : Vis.Absolute_Int;
         Draw_Rect       : Vis.Absolute.Rectangle_2d;
         Highlighting    : Vis_Data.Flags_Type;
      begin
         Highlighting := Vis_Data.Get_Highlighting (Node);
         Draw_Rect := Vis_Data.Get_Extent (Node);
         Move (Draw_Rect, -Origin);

         --  draw global highlight-borders if any
         for Light in Vis_Data.Global_Highlight_Type loop
            if Highlighting (Light) then
               Draw_Node_Highlighting
                 (Widget.Drawing.Node_Light (Light),
                  Default_Node_Light_Thickness,
                  Draw_Rect);
               Shrink (Draw_Rect, Default_Node_Light_Thickness);
            end if;
         end loop;

         --  draw node border
         Draw_Node_Border (Draw_Rect);
         Shrink (Draw_Rect, 1);

         --  count highlighting
         Light_Count := Lights'First;
         for Light in Vis_Data.Local_Highlight_Type loop
            if Highlighting (Light) then
               Lights (Light_Count) := Light;
               Light_Count := Light_Count + 1;
            end if;
         end loop;
         if Light_Count - Lights'First > 0 then
            --  draw filling using local highlighting
            Light_Rect := Draw_Rect;
            --  draw equally sized borders for each color
            Light_Thickness := Vis.Absolute_Int'Min
              (Get_Width (Light_Rect), Get_Height (Light_Rect)) /
              (2 * (Light_Count - Lights'First));

            --  draw first colors
            Light_Count := Light_Count - 1;
            while Light_Count > Lights'First loop
               Draw_Node_Highlighting
                 (Widget.Drawing.Node_Light (Lights (Light_Count)),
                  Light_Thickness,
                  Light_Rect);
               Shrink (Light_Rect, Light_Thickness);
               Light_Count := Light_Count - 1;
            end loop;
            --  draw last color
            Draw_Filled
              (Drawable  => Buffer,
               Gc        => Widget.Drawing.Node_Light (Lights (Light_Count)),
               Rectangle => Light_Rect,
               Origin    => Zero_2d);
         else
            --  draw filling without highlighting
            Draw_Node_Filling (Draw_Rect);
         end if;

         Inner_Rect := Draw_Rect;
      end Draw_Node_Rectangle;

      procedure Draw_Node_Content
        (Inner_Rect : in    Vis.Absolute.Rectangle_2d) is

         Icons_Width  : Glib.Gint := 0;
         Icons_Height : Glib.Gint := 0;

         procedure Draw_Icon
           (Icon   : in     Gdk.Pixmap.Gdk_Pixmap;
            Width  : in out Glib.Gint;
            Height : in out Glib.Gint) is

            use type Glib.Gint;
            Inner_Width  : Glib.Gint := Glib.Gint (Get_Width (Inner_Rect));
            Inner_Height : Glib.Gint := Glib.Gint (Get_Height (Inner_Rect));
            Too_Much     : Glib.Gint;
            X            : Glib.Gint;
            Y            : Glib.Gint;
         begin
            if Gdk."/=" (Icon, Gdk.Pixmap.Null_Pixmap) then
               X := Glib.Gint (Get_Left (Inner_Rect)) + Icons_Width;
               Y := Glib.Gint (Get_Top (Inner_Rect));

               Icons_Width := Icons_Width + Width;
               Icons_Height := Icons_Height + Height;

               if Icons_Width > Inner_Width then
                  Too_Much := Icons_Width - Inner_Width;
                  Width := Width - Too_Much;
                  Icons_Width := Inner_Width;
               end if;
               if Icons_Height > Inner_Height then
                  Too_Much := Icons_Height - Inner_Height;
                  Height := Height - Too_Much;
                  Icons_Height := Inner_Height;
               end if;
               if Width > 0 and then Height > 0 then
                  Gdk.Drawable.Draw_Pixmap
                    (Drawable => Buffer,
                     Gc       => Widget.Drawing.Node_Text,
                     Src      => Icon,
                     Xsrc     => 0,
                     Ysrc     => 0,
                     Xdest    => X,
                     Ydest    => Y,
                     Width    => Width,
                     Height   => Height);
               end if;
            else
               Drawing_Logger.Debug ("... no icon.");
            end if;
         end Draw_Icon;

         Font              : Gdk.Font.Gdk_Font :=
           Settings.Get_Node_Font (Widget);
         Icon              : Gdk.Pixmap.Gdk_Pixmap;
         Width             : Glib.Gint;
         Height            : Glib.Gint;
         Draw_Rect         : Vis.Absolute.Rectangle_2d := Inner_Rect;
         Icons_Bottom      : Vis.Absolute_Int;
         Id_Rect           : Vis.Absolute.Rectangle_2d;
         Class_Name_Rect   : Vis.Absolute.Rectangle_2d;
         Attrib_Name_Rect  : Vis.Absolute.Rectangle_2d;
         Attrib_Value_Rect : Vis.Absolute.Rectangle_2d;
         Iterator         : Graph_Lib.Node_Attribute_Filters.Filtered_Iterator;
         Attribute         : Graph_Lib.Node_Attribute_Id;
         Graph_Node        : Graph_Lib.Node_Id :=
           Vis_Data.Get_Graph_Node (Node);
         Graph_Node_Class  : Graph_Lib.Node_Class_Id :=
           Graph_Lib.Get_Node_Class_Id (Graph_Node);
         Line_Feed         : Vis.Absolute.Vector_2d :=
           Combine_Vector (0, Get_Height (Font) + Default_Text_Spacing);
      begin
         Gdk.GC.Set_Foreground
           (GC        => Widget.Drawing.Node_Text,
            Color     => Settings.Get_Node_Text_Color (Widget, Node));
         --  Icons
         Settings.Get_Node_Icon
           (Widget, Node,
            Icon, Width, Height);
         Draw_Icon (Icon, Width, Height);
         if Vis_Data.Is_Annotated (Node) then
            Settings.Get_Annotation_Icon
              (Widget,
               Icon, Width, Height);
            Draw_Icon (Icon, Width, Height);
         end if;
         Shrink (Draw_Rect, Default_Text_Spacing);

         Icons_Bottom := Get_Top (Draw_Rect) + Vis.Absolute_Int'Max
           (Vis.Absolute_Int (Icons_Height), Get_Height (Font));

         if Vis.Absolute_Int (Icons_Width) < Get_Width (Draw_Rect) then
            Id_Rect := Combine_Rectangle
              (X_1 => Get_Left (Draw_Rect) + Vis.Absolute_Int (Icons_Width),
               Y_1 => Get_Top (Draw_Rect),
               X_2 => Get_Right (Draw_Rect),
               Y_2 => Icons_Bottom);
            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Id_Rect,
               Text   => Graph_Lib.Node_Id_Image (Graph_Node));
         end if;

         --  Class name
         Class_Name_Rect := Combine_Rectangle
           (X_1 => Get_Left (Draw_Rect),
            Y_1 => Icons_Bottom + Default_Text_Spacing,
            X_2 => Get_Right (Draw_Rect),
            Y_2 => Icons_Bottom + Default_Text_Spacing + Get_Height (Font));

         Drawing_Logger.Debug ("... class name?");
         if Settings.Show_Node_Class_Name (Widget, Node) then
            Drawing_Logger.Debug
              ("... class name " & Vis.Absolute.Image (Class_Name_Rect) &
               " """ & Graph_Lib.Get_Node_Class_Tag (Graph_Node_Class) & '"');
            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Class_Name_Rect,
               Text   => Graph_Lib.Get_Node_Class_Tag (Graph_Node_Class));
         end if;

         --  Attributes
         Attrib_Name_Rect := Combine_Rectangle
           (Top_Left     => Get_Top_Left (Class_Name_Rect),
            Bottom_Right => Get_Bottom_Center (Class_Name_Rect) -
                              Combine_Vector (Default_Text_Spacing / 2, 0));
         Attrib_Value_Rect := Combine_Rectangle
           (Top_Left     => Get_Top_Center (Class_Name_Rect) +
                              Combine_Vector (Default_Text_Spacing / 2, 0),
            Bottom_Right => Get_Bottom_Right (Class_Name_Rect));

         Iterator := Settings.Get_Node_Attributes (Widget, Node);
         while Graph_Lib.Node_Attribute_Filters.More (Iterator) loop
            Move (Attrib_Name_Rect, Line_Feed);
            pragma Assert
              (Get_Bottom (Attrib_Name_Rect) <= Get_Bottom (Draw_Rect));
            Move (Attrib_Value_Rect, Line_Feed);

            Graph_Lib.Node_Attribute_Filters.Next (Iterator, Attribute);

            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Attrib_Name_Rect,
               Text   => Graph_Lib.Convert_Node_Attribute_Id_To_Name
                           (Attribute));
            Draw_Text
              (Buffer => Buffer,
               Font   => Font,
               Gc     => Widget.Drawing.Node_Text,
               Area   => Attrib_Value_Rect,
               Text   => Graph_Lib.Get_Node_Attribute_Value_As_String
                           (Graph_Node, Attribute));
         end loop;
      end Draw_Node_Content;

      Inner_Rect : Vis.Absolute.Rectangle_2d;
   begin
      Drawing_Logger.Debug ("Draw_Node");
      if Vis_Data.Is_Hidden (Node) then
         Drawing_Logger.Debug ("Hidden.");
         return;
      end if;

      Draw_Node_Rectangle (Inner_Rect);
      Draw_Node_Content (Inner_Rect);
   end Draw_Node;

   procedure Update_Buffer_Edges
     (Widget : access Graph_Widget_Record'Class) is

      package Clipping_Queues renames Vis_Data.Clipping_Queues;
      package Iterators renames Vis_Data.Edge_Update_Iterators;

      Clipping         : Vis_Data.Clipping_Queue_Access;
      Edges            : Iterators.Merger_Access;
      Current_Clipping : Vis_Data.Layer_Clipping_Access;
      Current_Edge     : Vis_Data.Vis_Edge_Id;
      Current_Layer    : Vis_Data.Layer_Type;
      Origin           : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      Vis_Data.Start_Edge_Refresh
        (Manager         => Widget.Manager,
         Display_Area    => Widget.Drawing.Buffer_Area,
         Clipping        => Clipping,
         Edges           => Edges,
         Refresh_Pending => True);

      declare
         Clip_Array : Layer_Clipping_Array
           (1 .. Clipping_Queues.Get_Size (Clipping.all));
         Clip_Count : Integer := Clip_Array'First;
      begin
         while Iterators.Has_More (Edges) loop
            Current_Edge := Iterators.Get_Current (Edges);
            Current_Layer := Vis_Data.Get_Layer (Current_Edge);

            loop
               --  open/close clip mask according to 'Current_Layer'
               exit when Clipping_Queues.Is_Empty (Clipping.all);
               Current_Clipping := Clipping_Queues.Get_Head (Clipping.all);
               exit when Vis_Data.Is_Below
                 (Current_Layer, Current_Clipping.Height);
               Apply_Clipping (Widget, Current_Clipping.all, Origin);
               Clipping_Queues.Remove_Head (Clipping.all);
               --  memorize 'Current_Clipping' to close the clip mask when done
               Clip_Array (Clip_Count) := Current_Clipping;
               Clip_Count := Clip_Count + 1;
            end loop;

            Draw_Edge
              (Widget, Widget.Drawing.Buffer, Current_Edge, Origin);

            Iterators.Forward (Edges);
         end loop;

         --  close clip mask
         while Clip_Count > Clip_Array'First loop
            Clip_Count := Clip_Count - 1;
            Revert_Clipping (Widget, Clip_Array (Clip_Count).all, Origin);
            Vis_Data.Free (Clip_Array (Clip_Count));
         end loop;
      end;

      Vis_Data.End_Edge_Refresh (Clipping, Edges);
   end Update_Buffer_Edges;

   procedure Update_Buffer_Nodes
     (Widget : access Graph_Widget_Record'Class) is

      package Clipping_Queues renames Vis_Data.Clipping_Queues;
      package Iterators renames Vis_Data.Node_Update_Iterators;

      Clipping         : Vis_Data.Clipping_Queue_Access;
      Nodes            : Iterators.Merger_Access;
      Current_Clipping : Vis_Data.Layer_Clipping_Access;
      Current_Node     : Vis_Data.Vis_Node_Id;
      Current_Layer    : Vis_Data.Layer_Type;
      Origin           : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      Drawing_Logger.Debug
        ("Update_Buffer_Nodes, Origin = " & Vis.Absolute.Image (Origin));
      Vis_Data.Start_Node_Refresh
        (Manager         => Widget.Manager,
         Display_Area    => Widget.Drawing.Buffer_Area,
         Clipping        => Clipping,
         Nodes           => Nodes,
         Refresh_Pending => True);

      declare
         Clip_Array : Layer_Clipping_Array
           (1 .. Clipping_Queues.Get_Size (Clipping.all));
         Clip_Count : Integer := Clip_Array'First;
      begin
         while Iterators.Has_More (Nodes) loop
            Current_Node := Iterators.Get_Current (Nodes);
            Current_Layer := Vis_Data.Get_Layer (Current_Node);

            loop
               --  open/close clip mask according to 'Current_Layer'
               exit when Clipping_Queues.Is_Empty (Clipping.all);
               Current_Clipping := Clipping_Queues.Get_Head (Clipping.all);
               exit when Vis_Data.Is_Below
                 (Current_Layer, Current_Clipping.Height);
               Drawing_Logger.Debug
                 ("... Apply_Clipping: " &
                  Vis.Absolute.Image (Current_Clipping.Area) & ", Origin: " &
                  Vis.Absolute.Image (Origin));
               Apply_Clipping (Widget, Current_Clipping.all, Origin);
               Clipping_Queues.Remove_Head (Clipping.all);
               --  memorize 'Current_Clipping' to close the clip mask when done
               Clip_Array (Clip_Count) := Current_Clipping;
               Clip_Count := Clip_Count + 1;
            end loop;

            Drawing_Logger.Debug
              ("... Draw_Node: " &
               Vis.Absolute.Image (Vis_Data.Get_Extent (Current_Node)));
            Draw_Node
              (Widget, Widget.Drawing.Buffer, Current_Node, Origin);

            Iterators.Forward (Nodes);
         end loop;

         --  close clip mask
         while Clip_Count > Clip_Array'First loop
            Clip_Count := Clip_Count - 1;
            Drawing_Logger.Debug
              ("... Revert_Clipping: " &
               Vis.Absolute.Image (Clip_Array (Clip_Count).Area) &
               ", Origin: " & Vis.Absolute.Image (Origin));
            Revert_Clipping (Widget, Clip_Array (Clip_Count).all, Origin);
            Vis_Data.Free (Clip_Array (Clip_Count));
         end loop;
      end;

      Vis_Data.End_Node_Refresh (Clipping, Nodes);
      Drawing_Logger.Debug ("... Nodes done.");
   end Update_Buffer_Nodes;


   procedure Update_Buffer_Background
     (Widget : access Graph_Widget_Record'Class) is

      package Rect_Lists renames Vis_Data.Rectangle_2d_Lists;
      Area          : Vis.Absolute.Rectangle_2d;
      Rectangles    : Rect_Lists.List;
      Iterator      : Rect_Lists.ListIter;
      Buffer_Origin : Vis.Absolute.Vector_2d :=
        Get_Top_Left (Widget.Drawing.Buffer_Area);
   begin
      Drawing_Logger.Debug ("Update_Buffer_Background");
      Vis_Data.Start_Refresh_Background
        (Manager         => Widget.Manager,
         Display_Area    => Widget.Drawing.Buffer_Area,
         Refresh_Area    => Rectangles,
         Refresh_Pending => True);

      Iterator := Rect_Lists.MakeListIter (Rectangles);
      while Rect_Lists.More (Iterator) loop
         Rect_Lists.Next (Iterator, Area);
         Drawing_Logger.Debug
           ("... Background rectangle: " & Vis.Absolute.Image (Area));
         Draw_Filled
           (Drawable  => Widget.Drawing.Buffer,
            Gc        => Widget.Drawing.Background,
            Rectangle => Area,
            Origin    => Buffer_Origin);
      end loop;

      Vis_Data.End_Refresh_Background (Rectangles);
      Drawing_Logger.Debug ("... Background done.");
   end Update_Buffer_Background;


   ----------------------------------------------------------------------------
   --  Updates the 'Buffer'
   procedure Update_Buffer
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Drawing_Logger.Debug ("Update_Buffer");
      Update_Buffer_Background (Widget);
      Update_Buffer_Edges (Widget);
      Update_Buffer_Nodes (Widget);
   end Update_Buffer;


   procedure Update_Display
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Absolute.Rectangle_2d) is
   begin
      Drawing_Logger.Debug ("Update_Display: " & Vis.Absolute.Image (Area));
      Update_Buffer (Widget);
      Gdk.Drawable.Copy_Area
        (Dest     => Widget.Drawing.Display,
         GC       => Widget.Drawing.Background,
         X        => 0,
         Y        => 0,
         Source   => Widget.Drawing.Buffer,
         Source_X => 0,
         Source_Y => 0,
         Width    => Glib.Gint
                       (Vis.Absolute.Get_Width (Widget.Drawing.Buffer_Area)),
         Height   => Glib.Gint
                       (Vis.Absolute.Get_Height (Widget.Drawing.Buffer_Area)));
      --  Update_Temporary (Widget);
   end Update_Display;


   ----------------------------------------------------------------------------
   --  Calculates the size of the display (desktop size + optimization)
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

   ----------------------------------------------------------------------------
   --  Calculates the size of the buffer (window size + optimization)
   procedure Calculate_Buffer_Size
     (Widget        : access Graph_Widget_Record'Class;
      Result_Width  :    out Glib.Gint;
      Result_Height :    out Glib.Gint) is

      Width         : Glib.Gint;
      Height        : Glib.Gint;
      Area          : Vis.Absolute.Rectangle_2d;
   begin
      Gdk.Window.Get_Size
        (Get_Window (Widget), Width, Height);
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
   end Calculate_Buffer_Size;

   ----------------------------------------------------------------------------
   --  Sets up the 'Background' gc
   procedure Set_Up_Background_Gc
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Background, Window);
      Gdk.GC.Set_Foreground
        (Widget.Drawing.Background, Settings.Get_Background_Color (Widget));
   end Set_Up_Background_Gc;

   ----------------------------------------------------------------------------
   --  Sets up all gcs for drawing nodes
   procedure Set_Up_Node_Gcs
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Border, Window);
--        Gdk.GC.Set_Clip_Mask
--          (Widget.Drawing.Node_Border, Widget.Drawing.Clip_Mask);
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Fill, Window);
--        Gdk.GC.Set_Clip_Mask
--          (Widget.Drawing.Node_Fill, Widget.Drawing.Clip_Mask);
      Gdk.GC.Gdk_New (Widget.Drawing.Node_Text, Window);
--        Gdk.GC.Set_Clip_Mask
--          (Widget.Drawing.Node_Text, Widget.Drawing.Clip_Mask);
      Gdk.GC.Set_Font
        (Widget.Drawing.Node_Text, Settings.Get_Node_Font (Widget));

      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Node_Light (I), Window);
--           Gdk.GC.Set_Clip_Mask
--             (Widget.Drawing.Node_Light (I), Widget.Drawing.Clip_Mask);
         Gdk.GC.Set_Foreground
           (Widget.Drawing.Node_Light (I),
            Settings.Get_Highlight_Color (Widget, I));
      end loop;
   end Set_Up_Node_Gcs;

   ----------------------------------------------------------------------------
   --  Sets up all gcs for drawing edges
   procedure Set_Up_Edge_Gcs
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Dashes : Glib.Guchar_Array :=
        (0 => Glib.Guchar (Default_Dash_Length),
         1 => Glib.Guchar (Default_Dash_Separation));
      Dots   : Glib.Guchar_Array :=
        (0 => Glib.Guchar (Default_Dot_Length),
         1 => Glib.Guchar (Default_Dot_Separation));
   begin
      for I in Edge_Style_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Edge_Line (I), Window);
--           Gdk.GC.Set_Clip_Mask
--             (Widget.Drawing.Edge_Line (I), Widget.Drawing.Clip_Mask);
      end loop;
      Gdk.GC.Set_Dashes
        (Gc          => Widget.Drawing.Edge_Line (Dashed_Line),
         Dash_Offset => Glib.Gint (Dashes'First),
         Dash_List   => Dashes);
      Gdk.GC.Set_Dashes
        (Gc          => Widget.Drawing.Edge_Line (Dotted_Line),
         Dash_Offset => Glib.Gint (Dots'First),
         Dash_List   => Dots);

      Gdk.GC.Gdk_New (Widget.Drawing.Edge_Label, Window);
--        Gdk.GC.Set_Clip_Mask
--          (Widget.Drawing.Edge_Label, Widget.Drawing.Clip_Mask);
      Gdk.GC.Set_Font
        (Widget.Drawing.Edge_Label, Settings.Get_Edge_Font (Widget));

      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Gdk_New (Widget.Drawing.Edge_Light (I), Window);
--           Gdk.GC.Set_Clip_Mask
--             (Widget.Drawing.Edge_Light (I), Widget.Drawing.Clip_Mask);
         Gdk.GC.Set_Foreground
           (Widget.Drawing.Edge_Light (I),
            Settings.Get_Highlight_Color (Widget, I));
      end loop;
   end Set_Up_Edge_Gcs;

   ----------------------------------------------------------------------------
   --  Sets up the clipping gcs
   procedure Set_Up_Clip_Gcs
     (Widget : access Graph_Widget_Record'Class) is

      Colormap : Gdk.Color.Gdk_Colormap := Get_Colormap (Widget);
      White    : Gdk.Color.Gdk_Color    := Gdk.Color.White (Colormap);
      Black    : Gdk.Color.Gdk_Color    := Gdk.Color.Black (Colormap);
   begin
      Gdk.GC.Gdk_New (Widget.Drawing.Clip_Open, Widget.Drawing.Clip_Mask);
      Gdk.GC.Set_Foreground (Widget.Drawing.Clip_Open, White);
--      Gdk.GC.Set_Function (Widget.Drawing.Clip_Open, Gdk.Types.Set);

      Gdk.GC.Gdk_New (Widget.Drawing.Clip_Close, Widget.Drawing.Clip_Mask);
      Gdk.GC.Set_Foreground (Widget.Drawing.Clip_Close, Black);
   end Set_Up_Clip_Gcs;



   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is

      Window : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Height : Glib.Gint;
      Width  : Glib.Gint;
   begin
      Calculate_Display_Size (Widget, Width, Height);
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Display,
         Window => Window,
         Width  => Width,
         Height => Height);
      Drawing_Logger.Debug
        ("Display size = " & Vis.Absolute.Image
         (Vis.Absolute.Combine_Vector
          (Vis.Absolute_Int (Width), Vis.Absolute_Int (Height))));

      Calculate_Buffer_Size (Widget, Width, Height);
      --  Buffer area, centered on (0, 0)
      Widget.Drawing.Buffer_Area := Combine_Rectangle
        (0, 0, Vis.Absolute_Int (Width) - 1, Vis.Absolute_Int (Height) - 1);
      Set_Center (Widget.Drawing.Buffer_Area, Zero_2d);
      --  Buffer
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Drawing.Buffer,
         Window => Window,
         Width  => Width,
         Height => Height);
      --  Clip mask
      Gdk.Bitmap.Gdk_New
        (Bitmap => Widget.Drawing.Clip_Mask,
         Window => Window,
         Width  => Width,
         Height => Height);

      Set_Up_Background_Gc (Widget);
      Set_Up_Edge_Gcs (Widget);
      Set_Up_Node_Gcs (Widget);
      Set_Up_Clip_Gcs (Widget);

      Clear (Widget.Drawing.Background, Widget.Drawing.Display);
      Clear (Widget.Drawing.Clip_Close, Widget.Drawing.Clip_Mask);

      --  Set Display as background pixmap
      Gdk.Window.Set_Back_Pixmap
        (Window          => Window,
         Pixmap          => Widget.Drawing.Display,
         Parent_Relative => False);
      --  Enqueue refresh
      States.Enable_Drawing (Widget);
      Queue_Draw (Widget);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.Disable_Drawing (Widget);

      Gdk.GC.Destroy (Widget.Drawing.Background);
      Gdk.GC.Destroy (Widget.Drawing.Clip_Open);
      Gdk.GC.Destroy (Widget.Drawing.Clip_Close);

      Gdk.GC.Destroy (Widget.Drawing.Node_Border);
      Gdk.GC.Destroy (Widget.Drawing.Node_Fill);
      Gdk.GC.Destroy (Widget.Drawing.Node_Text);
      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Node_Light (I));
      end loop;

      for I in Edge_Style_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Edge_Line (I));
      end loop;
      Gdk.GC.Destroy (Widget.Drawing.Edge_Label);
      for I in Vis_Data.Highlight_Type loop
         Gdk.GC.Destroy (Widget.Drawing.Edge_Light (I));
      end loop;

      Gdk.Bitmap.Unref (Widget.Drawing.Clip_Mask);
      Gdk.Pixmap.Unref (Widget.Drawing.Buffer);
      Gdk.Pixmap.Unref (Widget.Drawing.Display);
   end Shut_Down;

end Giant.Graph_Widgets.Drawing;
