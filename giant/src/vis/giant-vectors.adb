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
--  $RCSfile: giant-vectors.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------


package body Giant.Vectors is

   function "-"
     (Op : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Negate (Get_X (Op)), Coord_Negate (Get_Y (Op)));
   end "-";

   function "+"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Add (Left.X, Right.X), Coord_Add (Left.Y, Right.Y));
   end "+";

   function "-"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Vector_2d is
   begin
      return (Coord_Sub (Left.X, Right.X), Coord_Sub (Left.Y, Right.Y));
   end "-";

   function "*"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
      return Field_Type is
   begin
      return Field_Add
        (Vector_Mult_Coord (Left.X, Right.X),
         Vector_Mult_Coord (Left.Y, Right.Y));
   end "*";

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

   procedure Set_X
     (Vector : in out Vector_2d;
      X      : in     Coordinate_Type) is
   begin
      Vector.X := X;
   end Set_X;

   procedure Set_Y
     (Vector : in out Vector_2d;
      Y      : in     Coordinate_Type) is
   begin
      Vector.Y := Y;
   end Set_Y;


end Giant.Vectors;
