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
--  $RCSfile: giant-vectors.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--


generic

   type Field_Type is private;

   Field_Zero : Field_Type;

   with function Field_Add
     (A, B : in Field_Type)
      return Field_Type;
   with function Field_Sub
     (A, B : in Field_Type)
      return Field_Type;

   type Coordinate_Type is private;

   Coordinate_Zero : Coordinate_Type;

   with function Coord_Negate
     (A : in Coordinate_Type)
      return Coordinate_Type;
   with function Coord_Add
     (A, B : in Coordinate_Type)
      return Coordinate_Type;
   with function Coord_Sub
     (A, B : in Coordinate_Type)
      return Coordinate_Type;

   with function Scalar_Mult_Coord
     (A : in Field_Type; B : in Coordinate_Type)
      return Coordinate_Type;
   with function Vector_Mult_Coord
     (A : in Coordinate_Type; B : in Coordinate_Type)
      return Field_Type;

package Giant.Vectors is

   pragma Elaborate_Body;



   ---------------
   -- Vector_2d --
   ---------------

   type Vector_2d is private;

   Zero_2d : constant Vector_2d;

   function "-"
     (Op : in Vector_2d)
      return Vector_2d;

   function "+"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d;

   function "-"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d;

   function "*"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Field_Type;

   function Get_X
     (Vector : in Vector_2d)
      return Coordinate_Type;

   function Get_Y
     (Vector : in Vector_2d)
      return Coordinate_Type;

   function Combine_Vector
     (X      : in     Coordinate_Type;
      Y      : in     Coordinate_Type)
      return Vector_2d;

   procedure Set_X
     (Vector : in out Vector_2d;
      X      : in     Coordinate_Type);

   procedure Set_Y
     (Vector : in out Vector_2d;
      Y      : in     Coordinate_Type);


   ------------------
   -- Rectangle_2d --
   ------------------

   type Rectangle_2d is private;

   function Combine_Rectangle
     (X_1 : in     Coordinate_Type;
      Y_1 : in     Coordinate_Type;
      X_2 : in     Coordinate_Type;
      Y_2 : in     Coordinate_Type)
      return Rectangle_2d;

   function Combine_Rectangle
     (Top_Left     : in     Vector_2d;
      Bottom_Right : in     Vector_2d)
      return Rectangle_2d;

   function Get_Top
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

   function Get_Bottom
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

   function Get_Left
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

   function Get_Right
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

   function Get_Top_Left
     (Rectangle : in     Rectangle_2d)
      return Vector_2d;

   function Get_Top_Right
     (Rectangle : in     Rectangle_2d)
      return Vector_2d;

   function Get_Bottom_Left
     (Rectangle : in     Rectangle_2d)
      return Vector_2d;

   function Get_Bottom_Right
     (Rectangle : in     Rectangle_2d)
      return Vector_2d;

   function Get_Width
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

   function Get_Height
     (Rectangle : in     Rectangle_2d)
      return Coordinate_Type;

private

   type Vector_2d is
      record
         X : Coordinate_Type;
         Y : Coordinate_Type;
      end record;

   Zero_2d : constant Vector_2d := Combine_Vector
     (Coordinate_Zero, Coordinate_Zero);

   type Rectangle_2d is
      record
         Top    : Coordinate_Type;
         Left   : Coordinate_Type;
         Bottom : Coordinate_Type;
         Right  : Coordinate_Type;
      end record;

end Giant.Vectors;
