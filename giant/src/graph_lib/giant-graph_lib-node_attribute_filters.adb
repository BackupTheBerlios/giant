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
--  $RCSfile: giant-graph_lib-node_attribute_filters.adb,v $, $Revision: 1.6 $
--  $Author: koppor $
--  $Date: 2003/06/24 19:24:18 $
--
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body Giant.Graph_Lib.Node_Attribute_Filters is

   ---------------------------------------------------------------------------
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
      return Filter
   is
      Res          : Filter;
      Iter         : String_Lists.ListIter;
      Current_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Res  := new Filter_Record (Number_Of_Attributes => 1);

      Iter := STring_Lists.MakeListIter (Node_Attribute_Names_List);

      while String_Lists.More (Iter) loop
         String_Lists.Next (Iter, Current_Name);

      end loop;

      return Res;
   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy
     (Node_Attribute_Filter  : in out Filter)
   is
   begin
      --  Filter has not to be destroyed since it is a record
      null;
   end Destroy;

   ---------------------------------------------------------------------------
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter;
      Node                  : in Node_Id)
     return Filtered_Iterator
   is
   begin
      return Make_Filtered_Iter (Node_Attribute_Filter, Node);
   end Make_Filtered_Iter;

   ---------------------------------------------------------------------------
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean
   is
   begin
      return More (Iter);
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id)
   is
   begin
      null;
   end Next;

   ---------------------------------------------------------------------------
   procedure Reset
     (Iter   : in out Filtered_Iterator)
   is
   begin
      null;
   end Reset;

end Giant.Graph_Lib.Node_Attribute_Filters;

