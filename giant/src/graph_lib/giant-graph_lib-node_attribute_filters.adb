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
--  $RCSfile: giant-graph_lib-node_attribute_filters.adb,v $, $Revision: 1.10 $
--  $Author: koppor $
--  $Date: 2003/06/28 18:15:33 $
--
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Giant.Graph_Lib.Node_Attribute_Filters is

   ---------------------------------------------------------------------------
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
      return Filter
   is
      Res          : Filter;
      I            : Positive;
      Iter         : String_Lists.ListIter;
      Current_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Res  := new Filter_Type
        (1.. String_Lists.Length (Node_Attribute_Names_List));

      Iter := String_Lists.MakeListIter (Node_Attribute_Names_List);
      I    := 1;

      --  Fill Filter with Node_Attributes
      while String_Lists.More (Iter) loop
         String_Lists.Next (Iter, Current_Name);

         Res (I) :=
           Convert_Node_Attribute_Name_To_Id
           (Node_Class, Ada.Strings.Unbounded.To_String (Current_Name));

         I:=I+1;
      end loop;

      return Res;
   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy
     (Node_Attribute_Filter  : in out Filter)
   is

      procedure Free_Filter is new Ada.Unchecked_Deallocation
        (Filter_Type, Filter);

   begin
      Free_Filter (Node_Attribute_Filter);
   end Destroy;

   ---------------------------------------------------------------------------
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter)
     return Filtered_Iterator
   is
      Res : Filtered_Iterator;
   begin
      Res.Used_Filter      := Node_Attribute_Filter;
      Reset (Res);
      return Res;
   end Make_Filtered_Iter;

   ---------------------------------------------------------------------------
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean
   is
   begin
      return Iter.Current_Position <= Iter.Used_Filter'Last;
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id)
   is
   begin
      pragma Assert (More (Iter));
      Attrib := Iter.Used_Filter (Iter.Current_Position);
      Iter.Current_Position := Iter.Current_Position + 1;
   end Next;

   ---------------------------------------------------------------------------
   procedure Reset
     (Iter   : in out Filtered_Iterator)
   is
   begin
      Iter.Current_Position := Iter.Used_Filter'First;
   end Reset;

   ---------------------------------------------------------------------------
   function Size
     (Node_Attribute_Filter  : in Filter)
     return Natural
   is
   begin
      return Node_Attribute_Filter'Length;
   end Size;

end Giant.Graph_Lib.Node_Attribute_Filters;
