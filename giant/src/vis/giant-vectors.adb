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
--  $RCSfile: giant-vectors.adb,v $, $Revision: 1.9 $
--  $Author: keulsn $
--  $Date: 2003/07/02 12:44:50 $
--
------------------------------------------------------------------------------


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;

package body Giant.Vectors is

   function "-"
     (Op : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Negate (Get_X (Op)), Coord_Negate (Get_Y (Op)));
   end "-";
   pragma Inline ("-");

   function "+"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Add (Left.X, Right.X), Coord_Add (Left.Y, Right.Y));
   end "+";
   pragma Inline ("+");

   function "-"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Sub (Left.X, Right.X), Coord_Sub (Left.Y, Right.Y));
   end "-";
   pragma Inline ("-");

   function "*"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Field_Type is
   begin
      return Field_Add
        (Vector_Mult_Coord (Left.X, Right.X),
         Vector_Mult_Coord (Left.Y, Right.Y));
   end "*";
   pragma Inline ("*");

   function "*"
     (Left  : in Field_Type;
      Right : in Vector_2d)
     return Vector_2d is
   begin
      return
        (Scalar_Mult_Coord (Left, Right.X),
         Scalar_Mult_Coord (Left, Right.Y));
   end "*";
   pragma Inline ("*");

   function "/"
     (Left  : in Vector_2d;
      Right : in Field_Type)
     return Vector_2d is
   begin
      return
        (Scalar_Div_Coord (Left.X, Right),
         Scalar_Div_Coord (Left.Y, Right));
   end "/";
   pragma Inline ("/");

   function Get_X
     (Vector : in Vector_2d)
      return Coordinate_Type is
   begin
      return Vector.X;
   end Get_X;

   function Get_Y
     (Vector : in Vector_2d)
      return Coordinate_Type is
   begin
      return Vector.Y;
   end Get_Y;

   function Combine_Vector
     (X      : in     Coordinate_Type;
      Y      : in     Coordinate_Type)
      return Vector_2d is
   begin
      return (X, Y);
   end Combine_Vector;
   pragma Inline (Combine_Vector);

   procedure Set_X
     (Vector : in out Vector_2d;
      X      : in     Coordinate_Type) is
   begin
      Vector.X := X;
   end Set_X;
   pragma Inline (Set_X);

   procedure Set_Y
     (Vector : in out Vector_2d;
      Y      : in     Coordinate_Type) is
   begin
      Vector.Y := Y;
   end Set_Y;
   pragma Inline (Set_Y);

   function Image
     (Vector : in     Vector_2d)
     return String is
   begin
      return "(" & Image (Get_X (Vector)) & ", "
        & Image (Get_Y (Vector)) & ")";
   end Image;

   function Value
     (Image  : in     String)
     return Vector_2d is

      Trimmed_Image   : String := Ada.Strings.Fixed.Trim
        (Source => Image,
         Side   => Ada.Strings.Both);
      Separator_Index : Natural;
      X               : Coordinate_Type;
      Y               : Coordinate_Type;
   begin
      Put_Line (Image);
      Put_Line (Trimmed_Image);
      if Trimmed_Image (Trimmed_Image'First) /= '('
        or Trimmed_Image (Trimmed_Image'Last) /= ')' then

         raise Constraint_Error;
      end if;
      Separator_Index := Ada.Strings.Fixed.Index
        (Source  => Trimmed_Image,
         Pattern => ",");
      --  If "," not contained in Trimmed_Image then Separator_Index = 0
      --  In this case the following call must raise Constraint_Error.
      --  It is not necessary to raise explicitely.
      X := Value (Trimmed_Image
                   (Trimmed_Image'First + 1 .. Separator_Index - 1));
      Y := Value (Trimmed_Image
                   (Separator_Index + 1 .. Trimmed_Image'Last - 1));
      return Combine_Vector (X, Y);
   end Value;

   procedure Read_Vector
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Vector :    out Vector_2d) is

      X : Coordinate_Type;
      Y : Coordinate_Type;
   begin
      Read_Coordinate (Stream, X);
      Read_Coordinate (Stream, Y);
      Vector := Combine_Vector (X, Y);
   end Read_Vector;

   procedure Write_Vector
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Vector : in     Vector_2d) is
   begin
      Write_Coordinate (Stream, Get_X (Vector));
      Write_Coordinate (Stream, Get_Y (Vector));
   end Write_Vector;


   ----------------
   -- Rectangles --
   ----------------

   function Combine_Rectangle
     (X_1 : in     Coordinate_Type;
      Y_1 : in     Coordinate_Type;
      X_2 : in     Coordinate_Type;
      Y_2 : in     Coordinate_Type)
     return Rectangle_2d is

      Result : Rectangle_2d;
   begin
      if Coord_Less_Equal (X_1, X_2) then
         Result.Left  := X_1;
         Result.Right := X_2;
      else
         Result.Right := X_1;
         Result.Left  := X_2;
      end if;
      if Coord_Less_Equal (Y_1, Y_2) then
         Result.Top    := Y_1;
         Result.Bottom := Y_2;
      else
         Result.Bottom := Y_1;
         Result.Top    := Y_2;
      end if;
      return Result;
   end Combine_Rectangle;
   pragma Inline (Combine_Rectangle);

   function Combine_Rectangle
     (Top_Left     : in     Vector_2d;
      Bottom_Right : in     Vector_2d)
     return Rectangle_2d is

      Left   : Coordinate_Type := Get_X (Top_Left);
      Right  : Coordinate_Type := Get_X (Bottom_Right);
      Top    : Coordinate_Type := Get_Y (Top_Left);
      Bottom : Coordinate_Type := Get_Y (Bottom_Right);
   begin
      pragma Assert (Coord_Less_Equal (Left, Right));
      pragma Assert (Coord_Less_Equal (Top, Bottom));
      return Combine_Rectangle (Left, Top, Right, Bottom);
   end Combine_Rectangle;
   pragma Inline (Combine_Rectangle);

   function Get_Top
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Rectangle.Top;
   end Get_Top;
   pragma Inline (Get_Top);

   function Get_Bottom
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Rectangle.Bottom;
   end Get_Bottom;
   pragma Inline (Get_Bottom);

   function Get_Left
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Rectangle.Left;
   end Get_Left;
   pragma Inline (Get_Left);

   function Get_Right
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Rectangle.Right;
   end Get_Right;
   pragma Inline (Get_Right);

   function Get_Top_Left
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector (Get_Left (Rectangle), Get_Top (Rectangle));
   end Get_Top_Left;
   pragma Inline (Get_Top_Left);

   function Get_Top_Right
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector (Get_Right (Rectangle), Get_Top (Rectangle));
   end Get_Top_Right;
   pragma Inline (Get_Top_Right);

   function Get_Top_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector
        (X => Coord_Add
               (Get_Left (Rectangle),
                Scalar_Div_Coord
                  (Coord_Sub (Get_Right (Rectangle), Get_Left (Rectangle)),
                   To_Field_Type (2))),
         Y => Get_Top (Rectangle));
   end Get_Top_Center;
   pragma Inline (Get_Top_Center);

   function Get_Bottom_Left
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector (Get_Left (Rectangle), Get_Bottom (Rectangle));
   end Get_Bottom_Left;
   pragma Inline (Get_Bottom_Left);

   function Get_Bottom_Right
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector (Get_Right (Rectangle), Get_Bottom (Rectangle));
   end Get_Bottom_Right;
   pragma Inline (Get_Bottom_Right);

   function Get_Bottom_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Combine_Vector
        (X => Coord_Add
               (Get_Left (Rectangle),
                Scalar_Div_Coord
                  (Coord_Sub (Get_Right (Rectangle), Get_Left (Rectangle)),
                   To_Field_Type (2))),
         Y => Get_Bottom (Rectangle));
   end Get_Bottom_Center;
   pragma Inline (Get_Bottom_Center);

   function Get_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d is
   begin
      return Get_Top_Left (Rectangle) + Combine_Vector
        (Scalar_Div_Coord (Coord_Sub (Get_Width (Rectangle), Point_Size),
                           To_Field_Type (2)),
         Scalar_Div_Coord (Coord_Sub (Get_Height (Rectangle), Point_Size),
                           To_Field_Type (2)));
   end Get_Center;
   pragma Inline (Get_Center);

   function Get_Width
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Coord_Add (Coord_Sub (Get_Right (Rectangle),
                                   Get_Left (Rectangle)),
                        Point_Size);
   end Get_Width;
   pragma Inline (Get_Width);

   function Get_Height
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type is
   begin
      return Coord_Add (Coord_Sub (Get_Bottom (Rectangle),
                                   Get_Top (Rectangle)),
                        Point_Size);
   end Get_Height;
   pragma Inline (Get_Height);

   procedure Shrink
     (Rectangle : in out Rectangle_2d;
      Thickness : in     Coordinate_Type) is
   begin
      Set_Top
        (Rectangle,
         Coord_Add (Get_Top (Rectangle), Thickness));
      Set_Bottom
        (Rectangle,
         Coord_Add (Get_Bottom (Rectangle), Coord_Negate (Thickness)));
      Set_Left
        (Rectangle,
         Coord_Add (Get_Left (Rectangle), Thickness));
      Set_Right
        (Rectangle,
         Coord_Add (Get_Right (Rectangle), Coord_Negate (Thickness)));
   end Shrink;
   pragma Inline (Shrink);

   procedure Set_Top
     (Rectangle : in out Rectangle_2d;
      Top       : in     Coordinate_Type) is
   begin
      pragma Assert (Coord_Less_Equal (Top, Get_Bottom (Rectangle)));
      Rectangle.Top := Top;
   end Set_Top;
   pragma Inline (Set_Top);

   procedure Set_Bottom
     (Rectangle : in out Rectangle_2d;
      Bottom    : in     Coordinate_Type) is
   begin
      pragma Assert (Coord_Less_Equal (Get_Top (Rectangle), Bottom));
      Rectangle.Bottom := Bottom;
   end Set_Bottom;
   pragma Inline (Set_Bottom);

   procedure Set_Left
     (Rectangle : in out Rectangle_2d;
      Left      : in     Coordinate_Type) is
   begin
      pragma Assert (Coord_Less_Equal (Left, Get_Right (Rectangle)));
      Rectangle.Left := Left;
   end Set_Left;
   pragma Inline (Set_Left);

   procedure Set_Right
     (Rectangle : in out Rectangle_2d;
      Right     : in     Coordinate_Type) is
   begin
      pragma Assert (Coord_Less_Equal (Get_Left (Rectangle), Right));
      Rectangle.Right := Right;
   end Set_Right;
   pragma Inline (Set_Right);

   procedure Set_Top_Left
     (Rectangle : in out Rectangle_2d;
      Top_Left  : in     Vector_2d) is
   begin
      Set_Left (Rectangle, Get_X (Top_Left));
      Set_Top (Rectangle, Get_Y (Top_Left));
   end Set_Top_Left;
   pragma Inline (Set_Top_Left);

   procedure Set_Top_Right
     (Rectangle : in out Rectangle_2d;
      Top_Right : in     Vector_2d) is
   begin
      Set_Right (Rectangle, Get_X (Top_Right));
      Set_Top (Rectangle, Get_Y (Top_Right));
   end Set_Top_Right;
   pragma Inline (Set_Top_Right);

   procedure Set_Bottom_Left
     (Rectangle   : in out Rectangle_2d;
      Bottom_Left : in     Vector_2d) is
   begin
      Set_Left (Rectangle, Get_X (Bottom_Left));
      Set_Bottom (Rectangle, Get_Y (Bottom_Left));
   end Set_Bottom_Left;
   pragma Inline (Set_Bottom_Left);

   procedure Set_Bottom_Right
     (Rectangle    : in out Rectangle_2d;
      Bottom_Right : in     Vector_2d) is
   begin
      Set_Right (Rectangle, Get_X (Bottom_Right));
      Set_Bottom (Rectangle, Get_Y (Bottom_Right));
   end Set_Bottom_Right;
   pragma Inline (Set_Bottom_Right);

   procedure Set_Center
     (Rectangle : in out Rectangle_2d;
      Center    : in     Vector_2d) is
   begin
      Move (Rectangle, Center - Get_Center (Rectangle));
   end Set_Center;
   pragma Inline (Set_Center);

   procedure Move
     (Rectangle : in out Rectangle_2d;
      Offset    : in     Vector_2d) is

      Top_Left     : Vector_2d := Get_Top_Left (Rectangle);
      Bottom_Right : Vector_2d := Get_Bottom_Right (Rectangle);
   begin
      Rectangle := Combine_Rectangle
        (Top_Left + Offset, Bottom_Right + Offset);
   end Move;
   pragma Inline (Move);

   function Is_Inside
     (Rectangle : in     Rectangle_2d;
      Point     : in     Vector_2d)
     return Boolean is

      X : Coordinate_Type := Get_X (Point);
      Y : Coordinate_Type := Get_Y (Point);
   begin
      return Coord_Less_Equal (Get_Left (Rectangle), X)
        and then Coord_Less_Equal (Get_Top (Rectangle), Y)
        and then Coord_Less_Equal (X, Get_Right (Rectangle))
        and then Coord_Less_Equal (Y, Get_Bottom (Rectangle));
   end Is_Inside;
   pragma Inline (Is_Inside);

   function Intersects
     (First     : in     Rectangle_2d;
      Second    : in     Rectangle_2d)
     return Boolean is
   begin
      return Coord_Less_Equal (Get_Left (First), Get_Right (Second))
        and then Coord_Less_Equal (Get_Left (Second), Get_Right (First))
        and then Coord_Less_Equal (Get_Top (First), Get_Bottom (Second))
        and then Coord_Less_Equal (Get_Top (Second), Get_Bottom (First));
   end Intersects;
   pragma Inline (Intersects);

   function Image
     (Rectangle : in     Rectangle_2d)
     return String is
   begin
      return "(" & Image (Get_Top_Left (Rectangle)) & " - "
        & Image (Get_Bottom_Right (Rectangle)) & ")";
   end Image;

   procedure Read_Rectangle
     (Stream    : in     Bauhaus_IO.In_Stream_Type;
      Rectangle :    out Rectangle_2d) is

      Top_Left     : Vector_2d;
      Bottom_Right : Vector_2d;
   begin
      Read_Vector (Stream, Top_Left);
      Read_Vector (Stream, Bottom_Right);
      Rectangle := Combine_Rectangle (Top_Left, Bottom_Right);
   end Read_Rectangle;

   procedure Write_Rectangle
     (Stream    : in     Bauhaus_IO.Out_Stream_Type;
      Rectangle : in     Rectangle_2d) is
   begin
      Write_Vector (Stream, Get_Top_Left (Rectangle));
      Write_Vector (Stream, Get_Bottom_Right (Rectangle));
   end Write_Rectangle;

end Giant.Vectors;
