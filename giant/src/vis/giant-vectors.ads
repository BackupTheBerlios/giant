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
--  $RCSfile: giant-vectors.ads,v $, $Revision: 1.4 $
--  $Author: keulsn $
--  $Date: 2003/06/10 15:43:34 $
--
------------------------------------------------------------------------------


generic

   --  Field
   type Field_Type is private;

   --  Injective monotonic function Natural --> Field_Type
   with function To_Field_Type
     (A : in Natural)
     return Field_Type;

   --  Addition in Field
   with function Field_Add
     (A, B : in Field_Type)
     return Field_Type;
   --  Subtraction in Field
   with function Field_Sub
     (A, B : in Field_Type)
     return Field_Type;

   --  Coordinate for vector space
   type Coordinate_Type is private;

   --  Vector with all components = Coordinate_Zero is zero in vector space
   Coordinate_Zero : Coordinate_Type;

   --  Width and Height of a point (should be 0.0 or 1) affects width and
   --  height of rectangles
   Point_Size : Coordinate_Type;

   --  Text version of Coordinate_Type
   with function Image
     (A : in Coordinate_Type)
     return String;

   --  <= on Coordinate_Type
   with function Coord_Less_Equal
     (A : in Coordinate_Type; B : in Coordinate_Type)
     return Boolean;

   --  negation
   with function Coord_Negate
     (A : in Coordinate_Type)
     return Coordinate_Type;
   --  addition
   with function Coord_Add
     (A, B : in Coordinate_Type)
     return Coordinate_Type;
   --  subtraction
   with function Coord_Sub
     (A, B : in Coordinate_Type)
     return Coordinate_Type;

   --  Scalar multiplication
   with function Scalar_Mult_Coord
     (A : in Field_Type; B : in Coordinate_Type)
     return Coordinate_Type;
   --  Vector product for each coordinate
   with function Vector_Mult_Coord
     (A : in Coordinate_Type; B : in Coordinate_Type)
     return Field_Type;

   --  Scalar division for each coodinate
   with function Scalar_Div_Coord
     (A : in Coordinate_Type; B : in Field_Type)
     return Coordinate_Type;

   --  Read from Stream
   procedure Read_Coordinate
     (Stream     : in     Bauhaus_IO.In_Stream_Type;
      Coordinate :    out Coordinate_Type);

   --  Write to Stream
   with procedure Write_Coordinate
     (Stream     : in     Bauhaus_IO.Out_Stream_Type;
      Coordinate : in     Coordinate_Type);

package Giant.Vectors is

   pragma Elaborate_Body;


   ---------------
   -- Vector_2d --
   ---------------

   type Vector_2d is private;

   --  Zero in vector space
   Zero_2d : constant Vector_2d;

   --  Negation
   function "-"
     (Op : in Vector_2d)
     return Vector_2d;

   --  Addition
   function "+"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Vector_2d;

   --  Subtraction
   function "-"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Vector_2d;

   --  Inner product
   function "*"
     (Left  : in Vector_2d;
      Right : in Vector_2d)
     return Field_Type;

   --  Scalar multiplication
   function "*"
     (Left  : in Field_Type;
      Right : in Vector_2d)
     return Vector_2d;

   --  Scalar division
   function "/"
     (Left  : in Vector_2d;
      Right : in Field_Type)
     return Vector_2d;

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

   function Image
     (Vector : in     Vector_2d)
     return String;

   --  Read from Stream
   procedure Read_Vector
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Vector :    out Vector_2d);

   --  Write to Stream
   with procedure Write_Coordinate
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Vector : in     Vector_2d);



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

   function Get_Center
     (Rectangle : in     Rectangle_2d)
     return Vector_2d;

   function Get_Width
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;

   function Get_Height
     (Rectangle : in     Rectangle_2d)
     return Coordinate_Type;

   procedure Set_Top
     (Rectangle : in out Rectangle_2d;
      Top       : in     Coordinate_Type);

   procedure Set_Bottom
     (Rectangle : in out Rectangle_2d;
      Bottom    : in     Coordinate_Type);

   procedure Set_Left
     (Rectangle : in out Rectangle_2d;
      Left      : in     Coordinate_Type);

   procedure Set_Right
     (Rectangle : in out Rectangle_2d;
      Right     : in     Coordinate_Type);

   procedure Set_Top_Left
     (Rectangle : in out Rectangle_2d;
      Top_Left  : in     Vector_2d);

   procedure Set_Top_Right
     (Rectangle : in out Rectangle_2d;
      Top_Right : in     Vector_2d);

   procedure Set_Bottom_Left
     (Rectangle   : in out Rectangle_2d;
      Bottom_Left : in     Vector_2d);

   procedure Set_Bottom_Right
     (Rectangle    : in out Rectangle_2d;
      Bottom_Right : in     Vector_2d);

   procedure Set_Center
     (Rectangle : in out Rectangle_2d;
      Center    : in     Vector_2d);

   procedure Move
     (Rectangle : in out Rectangle_2d;
      Offset    : in     Vector_2d);

   function Image
     (Rectangle : in     Rectangle_2d)
     return String;

   --  Read from Stream
   procedure Read_Rectangle
     (Stream    : in     Bauhaus_IO.In_Stream_Type;
      Rectangle :    out Rectangle_2d);

   --  Write to Stream
   with procedure Write_Coordinate
     (Stream    : in     Bauhaus_IO.Out_Stream_Type;
      Rectangle : in     Rectangle_2d);

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
