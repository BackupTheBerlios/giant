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
--  $RCSfile: giant-layout_factory.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/07/07 12:10:32 $
--

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Matrix_Layouts;
with Giant.Tree_Layouts;
with Giant.String_Split;

with String_Lists;

package body Giant.Layout_Factory is

   ---------------------------------------------------------------------------
   procedure Create
     (Algorithm             : in     String;
      Selection_To_Layout   : in     Graph_Lib.Selections.Selection;
      Widget                : in     Graph_Widgets.Graph_Widget;
      Widget_Lock           : in     Graph_Widgets.Lock_Type;
      Target_Position       : in     Giant.Vis.Logic.Vector_2d;
      Additional_Parameters : in     String;
      Layout_Evolution      :    out Evolutions.Evolution_Class_Access)
   is

      -------------------------------------------------------------------------
      --  Parameters:
      --    Data: aren't parsed, since matrix doesn't get
      --          any additional parameters
      --          It is got here to ensure consistency with the calling of
      --          the other layouts
      procedure Parse_Matrix_Parameters
        (Data : in String)
      is
      begin
         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Matrix_Layouts.Initialize
            (Widget,
             Widget_Lock,
             Selection_To_Layout,
             Target_Position));
      end Parse_Matrix_Parameters;

      -------------------------------------------------------------------------
      --  Attention! - ";" may not be used within parameters!
      --    "Hello; you, you 2" is invalid, but "Hello you, you 2"
      --                                        is valid
      procedure Parse_Tree_Parameters
        (Data : in String)
      is

         ----------------------------------------------------------------------
         --  Parameters:
         --    Data - a string in the format "([<class set>[,<class set>]*]"
         --
         --  Returns:
         --    A meta class set containing all class sets
         function Convert_String_To_Meta_Class_Set
           (Data : in String)
           return Config.Class_Sets.Meta_Class_Set_Access
         is
            Class_Sets_List : String_Lists.List;
         begin
            Class_Sets_List := String_Split.Split_String
              (Source  => Data,
               Pattern => ",",
               Trim    => true);

            --  convert each item into a list
            --  if list.length = 0 return null
            --  else return converted list (into array)

            String_Lists.Destroy (Class_Sets_List);
            return Config.Class_Sets.Get_Class_Set_Access ("TBD: Stub");
         end Convert_String_To_Meta_Class_Set;

         Root_Node         : Giant.Graph_Lib.Node_Id;
         Meta_Class_Set    : Config.Class_Sets.Class_Set_Access;

         Parameters        : String_Lists.List;
         Parameters_Iter   : String_Lists.ListIter;
         Current_Parameter : Ada.Strings.Unbounded.Unbounded_String;

      begin
         --  Split parameters
         Parameters := String_Split.Split_String (Data, ";");

         --  Check amount of parameters
         if String_Lists.Length (Parameters) /= 2 then
            --  /= 2 is possible, since the list of classes
            --       is comma-separated and the parameter-list ";"-separated
            Layout_Evolution := null;
            Ada.Exceptions.Raise_Exception (Invalid_Format'Identity,
                                            "Not enough parameters");
         end if;

         Parameters_Iter := String_Lists.MakeListIter (Parameters);

         --  Get Root-Id
         String_Lists.Next (Parameters_Iter, Current_Parameter);
         begin
            Root_Node := Giant.Graph_Lib.Node_Id_Value
              (Ada.Strings.Unbounded.To_String (Current_Parameter));
         exception
            when Graph_Lib.Node_Does_Not_Exist =>
               Layout_Evolution := null;
               Ada.Exceptions.Raise_Exception (Invalid_Format'Identity,
                                               "Invalid root node");
         end;

         --  Class_sets
         String_Lists.Next (Parameters_Iter, Current_Parameter);
         Meta_Class_Set := Convert_String_To_Meta_Class_Set
           (Ada.Strings.Unbounded.To_String (Current_Parameter));

         Layout_Evolution := Giant.Evolutions.Evolution_Class_Access
           (Tree_Layouts.Initialize
            (Widget,
             Widget_Lock,
             Selection_To_Layout,
             Target_Position,
             Root_Node,
             Meta_Class_Set));
      end Parse_Tree_Parameters;

      Trimmed_Parameters : String :=
        Ada.Strings.Fixed.Trim (Additional_Parameters, Ada.Strings.Both);

   begin
      --  case for algorithms
      --  the called functions also set the out-parameters
      if Algorithm = "matrix" then
         Parse_Matrix_Parameters (Trimmed_Parameters);
      elsif Algorithm = "tree" then
         Parse_Tree_Parameters (Trimmed_Parameters);
      else
         Layout_Evolution := null;
         raise Unknown_Algorithm;
      end if;
   end Create;

end Giant.Layout_Factory;
