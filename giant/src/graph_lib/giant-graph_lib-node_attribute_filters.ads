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
--  $RCSfile: giant-graph_lib-node_attribute_filters.ads,v $, $Revision: 1.5 $
--  $Author: koppor $
--  $Date: 2003/06/24 19:24:25 $
--
------------------------------------------------------------------------------
--
--  Offers filtering used for visulaization

--  from Bauhaus
with String_Lists;

package Giant.Graph_Lib.Node_Attribute_Filters is

   type Filter is private;
   type Filtered_Iterator is private;

   ------------------------------------
   --  Create & Destroy of a filter  --
   ------------------------------------

   --  -----------------------------------------------------------------------
   --  If a given attribute does not exist in Node_Class of the given filter,
   --    it is ignored
   --
   --  Params:
   --    Node_Class:                The node class the filter created for
   --    Ndoe_Attribute_Names_List: The list of classes, which should be shown
   --                               (i.e. NOT filtered!)
   --  Returns:
   --    The corresponding filter
   --
   --  Raises:
   --    Node_Attribute_Does_Not_Exist:
   --      If a Node_Attribute given by given names does not exist
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
     return Filter;

   --  -----------------------------------------------------------------------
   procedure Destroy
      (Node_Attribute_Filter  : in out Filter);

   ----------------------------------------
   --  Iterator - analogue to lists.ads  --
   ----------------------------------------

   ---------------------------------------------------------------------------
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter;
      Node                  : in Node_Id)
     return Filtered_Iterator;

   ---------------------------------------------------------------------------
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id);

   ---------------------------------------------------------------------------
   --  Sets the iterator to the first element
   procedure Reset
     (Iter : in out Filtered_Iterator);

private
   type Filter_Record
     (Number_Of_Attributes : Positive) is
   record
      Attributes : Node_Attribute_Id_Array (1 .. Number_Of_Attributes);
   end record;

   type Filter is access Filter_Record;

   type Filtered_Iterator is null record;

   --  taken from mail from keulsn, 20030426
   --
   --  [...Zoom_Level... not to be implemented, 13-06-2003 keulsn]
   --  type Attribute_Filter_Type is array (Positive range <>) of Boolean;
   --  pragma Pack (Attribute_Filter_Type);
   --  [Since there is a Node_Attribute_Id, implementation as makes far
   --   more sense, 13-06-2003 keulsn]
   --  type Attribute_Filter_Access is access Attribute_Filter_Type;
   --  [...Filter_For_Zoom... not to be implemented, 13-06-2003 keulsn]
end Giant.Graph_Lib.Node_Attribute_Filters;
