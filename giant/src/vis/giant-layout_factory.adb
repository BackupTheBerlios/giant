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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-layout_factory.adb,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/07/01 21:50:30 $
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Giant.Matrix_Layouts;
with Giant.Tree_Layouts;
with Giant.Vis;
with Giant.String_Split;

with String_Lists;

package body Giant.Layout_Factory is

   ---------------------------------------------------------------------------
   procedure Create
     (Algorithm                           :
        in     String;
      Selection_To_Layout                 :
        in     Giant.Graph_Lib.Selections.Selection;
      Widget                              :
        in     Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock                         :
        in     Giant.Graph_Widgets.Lock_Type;
      Additional_Parameters               :
        in     String;
      Additional_Parameters_Verbose_Error :
        out    String;
      Layout_Evolution                    :
        out Giant.Evolutions.Evolution_Class_Access)
   is

      Invalid_Format : exception;

      -------------------------------------------------------------------------
      --  Format:
      --    (x, y)
      --
      --  To be optimized, if x or y contain ","
      --    Parse into String-list and separte in two halfs and combine
      --    results again
      --    convert these two parts and voila, there it is
      function Get_Vector_2d
        (The_Data : in String)
        return Giant.Vis.Logic.Vector_2d
      is

         function Get_Logic_Float
           (S : in String)
           return Giant.Vis.Logic_Float
         is
            N : Natural;
         begin
            begin
               N := Natural'Value (S);
            exception
               when others =>
                  raise Invalid_Format;
            end;

            return Giant.Vis.To_Logic_Float (N);
         end Get_Logic_Float;

         Data : String := Ada.Strings.Fixed.Trim
           (The_Data, Ada.Strings.Both);

         Pos_Comma : Natural;

         Vector_X  : Giant.Vis.Logic_Float;
         Vector_Y  : Giant.Vis.Logic_Float;
      begin
         if (Data (Data'First) /= '(') or
           (Data (Data'Last) /= ')') then
            raise Invalid_Format;
         end if;

         Pos_Comma := Ada.Strings.Fixed.Index (Data, ",");
         if Pos_Comma = 0 then
            raise Invalid_Format;
         end if;

         Vector_X := Get_Logic_Float
           (Data (Data'First .. Pos_Comma-1));
         Vector_Y := Get_Logic_Float
           (Data (Pos_Comma + 1 .. Data'Length));

         return Giant.Vis.Logic.Combine_Vector (Vector_X, Vector_Y);
      end Get_Vector_2d;

      -------------------------------------------------------------------------
      procedure Parse_Matrix_Parameters
        (Data : in String)
      is
         Target_Position : Giant.Vis.Logic.Vector_2d;
      begin
         begin
            Target_Position := Get_Vector_2d (Additional_Parameters);
         exception
            when Invalid_Format =>
               Additional_Parameters_Verbose_Error :=
                 "Wrong format at Target_Position";
         end;

         Additional_Parameters_Verbose_Error := "";

         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Matrix_Layouts.Initialize
            (Widget,
             Widget_Lock,
             Selection_To_Layout,
             Target_Position));
      end Parse_Matrix_Parameters;

      -------------------------------------------------------------------------
      --  Attention! - ";" may not be used within parameters!
      --    ("Hello; you", "you 2") is invalid, but ("Hello you", "you 2")
      --                                            is valid
      procedure Parse_Tree_Parameters
        (Data : in String)
      is
         Root_Node       : Giant.Graph_Lib.Node_Id;
         Target_Position : Giant.Vis.Logic.Vector_2d;
         --  Class_Set_

         Parameters        : String_Lists.List;
         Iter              : String_Lists.ListIter;
         Current_Parameter : Ada.Strings.Unbounded.Unbounded_String;

      begin
         --  Split parameters
         Parameters := String_Split.Split_String (Data, ";");

         --  Check amount of parameters
         if String_Lists.Length (Parameters) /= 3 then
            --  /= 3 is possible, since the list of classes
            --       is comma-separated and the parameter-list ";"-separated
            Additional_Parameters_Verbose_Error := "Not enough parameters";
            Layout_Evolution := null;
            return;
         end if;

         Iter := String_Lists.MakeListIter (Parameters);

         --  Get Root-Id
         String_Lists.Next (Iter, Current_Parameter);
         begin
            Root_Node := Giant.Graph_Lib.Node_Id_Value
              (Ada.Strings.Unbounded.To_String (Current_Parameter));
         exception
            when others =>
               Additional_Parameters_Verbose_Error :=
                 "Problem with root_node";
               Layout_Evolution := null;
               return;
         end;

         --  Get Target_Position
         String_Lists.Next (Iter, Current_Parameter);
         begin
            Target_Position := Get_Vector_2d
              (Ada.Strings.Unbounded.To_String (Current_Parameter));
         exception
            when Invalid_Format =>
               Additional_Parameters_Verbose_Error :=
                 "Wrong format at Target_Position";
         end;

         Additional_Parameters_Verbose_Error := "";

         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Tree_Layouts.Initialize
            (Widget,
             Widget_Lock,
             Selection_To_Layout,
             Target_Position,
             Root_Node));
      end Parse_Tree_Parameters;

   begin
      --  case for algorithms
      --  the called functions also set the out-parameters
      if Algorithm = "Matrix" then
         Parse_Matrix_Parameters
           (Ada.Strings.Fixed.Trim (Additional_Parameters, Ada.Strings.Both));
      elsif Algorithm = "Tree" then
         Parse_Tree_Parameters
           (Ada.Strings.Fixed.Trim (Additional_Parameters, Ada.Strings.Both));
      else
         Additional_Parameters_Verbose_Error := "";
         Layout_Evolution := null;
         raise Unknown_Algorithm;
      end if;
   end Create;

end Giant.Layout_Factory;
