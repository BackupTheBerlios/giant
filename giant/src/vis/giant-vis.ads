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
--  $RCSfile: giant-vis.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--


with Giant.Vectors;

package Giant.Vis is

   subtype Logic_Float is Float;

   package Logic is new Vectors
     (Field_Type        => Logic_Float,
      Field_Zero        => 0.0,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Logic_Float,
      Coordinate_Zero   => 0.0,
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*");

   subtype Zoom_Level is Float;

   subtype Absolute_Int is Integer;

   subtype Absolute_Natural is Absolute_Int range 0 .. Absolute_Int'Last;

   package Absolute is new Vectors
     (Field_Type        => Absolute_Int,
      Field_Zero        => 0,
      Field_Add         => "+",
      Field_Sub         => "-",
      Coordinate_Type   => Absolute_Int,
      Coordinate_Zero   => 0,
      Coord_Negate      => "-",
      Coord_Add         => "+",
      Coord_Sub         => "-",
      Scalar_Mult_Coord => "*",
      Vector_Mult_Coord => "*");

private

   function Transform
     (Point  : in     Logic.Vector_2d;
      Origin : in     Logic.Vector_2d;
      Zoom   : in     Zoom_Level)
      return Absolute.Vector_2d;

end Giant.Vis;
