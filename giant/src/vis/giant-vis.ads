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
--  $RCSfile: giant-vis.ads,v $, $Revision: 1.3 $
--  $Author: keulsn $
--  $Date: 2003/06/09 01:13:39 $
--
------------------------------------------------------------------------------


with Glib;

with Giant.Vectors;

package Giant.Vis is

   subtype Logic_Float is Float;

   function To_Logic_Float
     (A : in Natural)
     return Logic_Float;

   function Logic_Float_Image
     (A : in Logic_Float)
     return String;

   package Logic is new Vectors
     (Field_Type        => Logic_Float,
      To_Field_Type     => To_Logic_Float,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Logic_Float,
      Coordinate_Zero   => 0.0,
      Point_Size        => 0.0,
      Image             => Logic_Float_Image,
      Coord_Less_Equal  => "<=",
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*",
      Scalar_Div_Coord  => "/");

   subtype Zoom_Level is Float;

   subtype Absolute_Int is Integer range
     Integer (Glib.Gint'First) .. Integer (Glib.Gint'Last);

   subtype Absolute_Natural is Absolute_Int range 0 .. Absolute_Int'Last;

   function To_Absolute_Int
     (A : in Natural)
     return Absolute_Int;

   package Absolute is new Vectors
     (Field_Type        => Absolute_Int,
      To_Field_Type     => To_Absolute_Int,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Absolute_Int,
      Coordinate_Zero   => 0,
      Point_Size        => 1,
      Image             => Absolute_Int'Image,
      Coord_Less_Equal  => "<=",
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*",
      Scalar_Div_Coord  => "/");


   type Transformation_Type is private;

   function To_Absolute
     (Vector : in     Logic.Vector_2d)
     return Absolute.Vector_2d;

   function To_Logic
     (Vector : in     Absolute.Vector_2d)
     return Logic.Vector_2d;

   function Transform
     (Point          : in     Logic.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Absolute.Vector_2d;

   function Transform_Backward
     (Point          : in     Absolute.Vector_2d;
      Origin         : in     Logic.Vector_2d;
      Zoom           : in     Zoom_Level)
     return Logic.Vector_2d;

   function Transform
     (Transformation : in     Transformation_Type;
      Point          : in     Logic.Vector_2d)
     return Absolute.Vector_2d;

   function Transform_Backward
     (Transformation : in     Transformation_Type;
      Point          : in     Absolute.Vector_2d)
     return Logic.Vector_2d;

   function Transform
     (Transformation : in     Transformation_Type;
      Source_Rect    : in     Logic.Rectangle_2d)
     return Absolute.Rectangle_2d;

   procedure Transform_To_Gdk
     (Point          : in     Logic.Vector_2d;
      Transformation : in     Transformation_Type;
      X              :    out Glib.Gint;
      Y              :    out Glib.Gint);

   function Get_Transformation_Rect_Into_Rect_Centered
     (Source         : in     Logic.Rectangle_2d;
      Target         : in     Absolute.Rectangle_2d)
     return Transformation_Type;

private

   type Transformation_Type is
      record
         Origin : Logic.Vector_2d := Logic.Combine_Vector (0.0, 0.0);
         Zoom   : Zoom_Level      := 1.0;
      end record;

end Giant.Vis;
