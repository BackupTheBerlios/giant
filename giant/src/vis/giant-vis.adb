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
--  $RCSfile: giant-vis.adb,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/09 01:13:39 $
--
------------------------------------------------------------------------------


package body Giant.Vis is

   use type Absolute.Vector_2d;
   use type Logic.Vector_2d;

   function To_Logic_Float
     (A : in Natural)
     return Logic_Float is
   begin
      return Logic_Float (A);
   end To_Logic_Float;

   function Logic_Float_Image
     (A : in Logic_Float)
     return String is

      Print_Minus    : Boolean     := A < 0.0;
      Positive_Value : Logic_Float := abs A;
      Trunc          : Logic_Float := Logic_Float'Truncation (Positive_Value);
      More           : Logic_Float := Logic_Float'Truncation
                                        ((Positive_Value - Trunc) * 1_000.0);
      Fore_Int       : Integer;
      Aft_Int        : Integer;
   begin
      if Logic_Float (Integer'Last) < Trunc then
         if Print_Minus then
            return "-???";
         else
            return "+???";
         end if;
      else
         Fore_Int := Integer (Trunc);
         Aft_Int  := Integer (More);
         declare
            Fore      : String          := Integer'Image (Fore_Int);
            Aft_Image : String          := Integer'Image (Aft_Int);
            Aft       : String (1 .. 3) := (others => '0');
         begin
            Aft (Aft'Last - Aft_Image'Length + 2 .. Aft'Last) :=
              Aft_Image (Aft_Image'First + 1 .. Aft_Image'Last);
            if Print_Minus then
               return "-" & Fore (Fore'First + 1 .. Fore'Last) & "." & Aft;
            else
               return Fore (Fore'First + 1 .. Fore'Last) & "." & Aft;
            end if;
         end;
      end if;
   end Logic_Float_Image;

   function To_Absolute_Int
     (A : in Natural)
     return Absolute_Int is
   begin
      return A;
   end To_Absolute_Int;

   function To_Absolute
     (Vector : in     Logic.Vector_2d)
     return Absolute.Vector_2d is
   begin
      return Absolute.Combine_Vector
        (Absolute_Int (Logic.Get_X (Vector)),
         Absolute_Int (Logic.Get_Y (Vector)));
   end To_Absolute;

   function To_Logic
     (Vector : in     Absolute.Vector_2d)
     return Logic.Vector_2d is
   begin
      return Logic.Combine_Vector
        (Logic_Float (Absolute.Get_X (Vector)),
         Logic_Float (Absolute.Get_Y (Vector)));
   end To_Logic;

   function Transform
     (Point          : in     Logic.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Absolute.Vector_2d is
   begin
      return To_Absolute (Zoom * (Point + Origin));
   end Transform;

   function Transform_Backward
     (Point          : in     Absolute.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Logic.Vector_2d is
   begin
      if Zoom > 0.0 then
         return To_Logic (Point) / Zoom - Origin;
      else
         return Logic.Zero_2d;
      end if;
   end Transform_Backward;

   function Transform
     (Transformation : in     Transformation_Type;
      Point          : in     Logic.Vector_2d)
     return Absolute.Vector_2d is
   begin
      return Transform (Point, Transformation.Origin, Transformation.Zoom);
   end Transform;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Point          : in     Absolute.Vector_2d)
     return Logic.Vector_2d is
   begin
      return Transform_Backward
        (Point, Transformation.Origin, Transformation.Zoom);
   end Transform_Backward;

   function Transform
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d is
   begin
      return Absolute.Combine_Rectangle
        (Top_Left     => Transform (Transformation,
                                    Logic.Get_Top_Left (Source_Rect)),
         Bottom_Right => Transform (Transformation,
                                    Logic.Get_Bottom_Right (Source_Rect)));
   end Transform;

   procedure Transform_To_Gdk
     (Point          : in     Logic.Vector_2d;
      Transformation : in     Transformation_Type;
      X              :    out Glib.Gint;
      Y              :    out Glib.Gint) is

      Result_Point : Absolute.Vector_2d;
   begin
      Result_Point := Transform (Transformation, Point);
      X := Glib.Gint (Absolute.Get_X (Result_Point));
      Y := Glib.Gint (Absolute.Get_Y (Result_Point));
   end Transform_To_Gdk;

   function Get_Transformation_Rect_Into_Rect_Centered
     (Source         : in     Logic.Rectangle_2d;
      Target         : in     Absolute.Rectangle_2d)
     return Transformation_Type is

      Source_Width  : Logic_Float := Logic.Get_Width (Source);
      Source_Height : Logic_Float := Logic.Get_Height (Source);
      Target_Width  : Logic_Float :=
        Logic_Float (Absolute.Get_Width (Target));
      Target_Height : Logic_Float :=
        Logic_Float (Absolute.Get_Height (Target));

      Origin        : Logic.Vector_2d;
      X_Zoom        : Zoom_Level;
      Y_Zoom        : Zoom_Level;
      Zoom          : Zoom_Level;
   begin
      if Source_Width <= 0.0 or Source_Height <= 0.0 then
         return (Origin => Logic.Zero_2d, Zoom => 0.0);
      else
         X_Zoom := Zoom_Level (Target_Width / Source_Width);
         Y_Zoom := Zoom_Level (Target_Height / Source_Height);
         if X_Zoom <= 0.0 then
            Zoom := Y_Zoom;
         elsif Y_Zoom <= 0.0 then
            Zoom := X_Zoom;
         else
            Zoom := Zoom_Level'Min (X_Zoom, Y_Zoom);
         end if;
         if Zoom <= 0.0 then
            return (Origin => Logic.Zero_2d, Zoom => 0.0);
         else
            Origin := To_Logic (Absolute.Get_Center (Target)) / Zoom
              - Logic.Get_Center (Source);
            return (Origin, Zoom);
         end if;
      end if;
   end Get_Transformation_Rect_Into_Rect_Centered;

end Giant.Vis;
