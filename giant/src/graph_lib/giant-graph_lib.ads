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
--  $RCSfile: giant-graph_lib.ads,v $, $Revision: 1.9 $
--  $Author: koppor $
--  $Date: 2003/06/12 14:42:38 $
--

--  Bauhaus / IML
with IML_Reflection;
with Storables;
with SLocs; --  used at private part

--  Bauhaus / Reuse
--  with String_Lists;
with Ordered_Sets;
with Lists;

pragma Elaborate_All (Ordered_Sets);
pragma Elaborate_All (Lists);

package Giant.Graph_Lib is

   ------------------------------------
   --  Basic-Types:                  --
   --    Nodes, Attributes of Nodes  --
   ------------------------------------

   ---------------------------------------------------------------------------
   --  to be considered private
   --  is here to simplify implementation
   type Node_Record (Number_Of_Incoming_Edges : Natural;
                     Number_Of_Outgoing_Edges : Natural) is limited private;

   ---------------------------------------------------------------------------
   --  unique Id of one single node in the duplicated IML-Graph
   --
   --  It is constant throughout one runtime
   --  "access constant" could not be used, since an unchecked_deallocation
   --  is impossible then
   --
   --  Node_Id_Image is invariant over multiple runs of GIANT
   type Node_Id is access Node_Record;


   ---------------------------------------------------------------------------
   --  unique Id of one class to which a node is belonging to
   --  ("type of the node")
   --
   --    'Node_Class_Id' is declared as a subtype only to simplify the
   --    implementation of the package. It is to be considered a private type.
   --    Used for casting complete arrays
   subtype Node_Class_Id is IML_Reflection.Class_ID;

   ---------------------------------------------------------------------------
   --  Unique id of one single attribute "type" which appears in
   --    one or more nodes ("type of the attribute")
   --
   --    'Node_Attribute_Id' is declared as a subtype only to simplify
   --    implementation of the package. It is to be considered a private type.
   --    Used for casting complete arrays
   subtype Node_Attribute_Id is IML_Reflection.Field_ID;

   ---------------------------------------------------------------------------
   --  Id of one class to which an attribute may belong to
   --    ("Type of the attribute")
   --  Mirror of IML-Fields
   type Node_Attribute_Class_Id is
      (Class_Node_Id,
       Class_Node_Id_List,
       Class_Node_Id_Set,
       --  Class_String,

       --  still don't know how to deal with Enumerator_Field
       Class_String_List,

       Class_SLoc,
       Class_Boolean,
       Class_Natural);

   ---------------------------
   --  Basic-Types          --
   --    Edges              --
   --                       --
   --    Analogue to Nodes  --
   ---------------------------

   ---------------------------------------------------------------------------
   type Edge_Record is limited private;

   ---------------------------------------------------------------------------
   --  unique Id of one single edge in the duplicated IML-Graph
   type Edge_Id is access all Edge_Record;

   ---------------------------------------------------------------------------
   --  to be considered private
   --  is here to simplify implementation
   type Edge_Class is limited private;

   ---------------------------------------------------------------------------
   --  unique ID of one class to which an edge may belong to
   --
   --    This is to be considered private
   type Edge_Class_Id is access all Edge_Class;


   -----------------------------------------------------------------------
   --  Edge_Attribute is not implemented, since it is not supported by  --
   --  IML_Reflection                                                   --
   -----------------------------------------------------------------------

   -----------------
   --  Iterators  --
   -----------------

   ---------------------------------------------------------------------------
   --  Desc:
   --    An iterator for iterating on the attributes of a single node
   type Node_Attribute_Iterator is private;


   -----------------
   --  Constants  --
   -----------------

   ---------------------------------------------------------------------------
   Invalid_Node_Id                : constant Node_Id;
   Invalid_Attribute_Value_String : constant String;

   ------------------
   --  Exceptions  --
   ------------------

   Load_Error                    : exception;
   Node_Does_Not_Exist           : exception;
   Node_Class_Does_Not_Exist     : exception;
   Node_Attribute_Does_Not_Exist : exception;
   Edge_Class_Does_Not_Exist     : exception;

   Wrong_Attribute_Type          : exception;

   -------------------
   --  Comparators  --
   -------------------

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Id;
       Right : Node_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Class_Id;
       Right : Node_Class_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "="
      (Left  : Node_Class_Id;
       Right : Node_Class_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Edge_Id;
       Right : Edge_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Edge_Class_Id;
       Right : Edge_Class_Id)
      return Boolean;

   -------------
   --  Lists  --
   -------------
   package Node_Id_Lists is
      new Lists (ItemType => Node_Id);
   subtype Node_Id_List is Node_Id_Lists.List;

   ------------
   --  Sets  --
   ------------

   -----------------------------------------------------------------------
   --  This package provides set and iterator operations. The
   --  functionality is not reimplented but offered using Ordered_Sets.
   --
   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Id_Set is Node_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  This package provides set and iterator operations. The
   --  functionality is not reimplented but offered using Ordered_Sets.
   --
   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Attribute_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Attribute_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Attribute_Class_Id_Set is
      Node_Attribute_Class_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  This package provides set and iterator operations. The
   --  functionality is not reimplented but offered using Ordered_Sets.
   --
   --  Read and Write will not be used and therefore are not
   --  instantiated.
   package Node_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Node_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Node_Class_Id_Set is Node_Class_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  Set and iterator operations are offered by Ordered_Set
   --
   --  Read and Write are not instantiated, since they won't be used
   package Edge_Id_Sets is
      new Ordered_Sets (Item_Type => Edge_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Edge_Id_Set is Edge_Id_Sets.Set;

   ---------------------------------------------------------------------------
   --  Set and iterator operations are offered by Ordered_Set
   --
   --  Read and Write are not instantiated, since they won't be used
   package Edge_Class_Id_Sets is
      new Ordered_Sets (Item_Type => Edge_Class_Id,
                        "="       => "=",
                        "<"       => "<");
   subtype Edge_Class_Id_Set is Edge_Class_Id_Sets.Set;


   ------------------------
   --  Create & Destroy  --
   ------------------------

   ---------------------------------------------------------------------------
   --  Initializes the graph
   --
   --  Raises:
   --    Load_Error if something has gone wrong
   procedure Create (Path_To_IML_File : in String);

   ---------------------------------------------------------------------------
   --  Unloads the graph from the memory
   procedure Destroy;


   ----------------------
   -- Existance-Checks --
   ----------------------

   ---------------------------------------------------------------------------
   --  Checks if given Node_Class exists anywhere in the loaded IML-Graph
   function Does_Node_Class_Exist
      (Node_Class_Name : in String)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Checks if given Node_Attribute exists at given class
   --
   --  Returns:
   --    True  - if given attribute exists
   --    False - if it does not exist or if given Class does not exist
   function Does_Node_Attribute_Exist
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Checks if given Node_Class has an
   --    attribute Attribute_Name representing an edge
   function Does_Edge_Class_Exist
      (Node_Class     : in Node_Class_Id;
       Node_Attribute : in Node_Attribute_Id)
      return Boolean;

   ----------------------
   -- Type conversions --
   ----------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    The name belonging to given Node_Attribute
   function Convert_Node_Attribute_Id_To_Name
      (Node_Attribute : in Node_Attribute_Id)
      return String;

   ---------------------------------------------------------------------------
   --  Returns:
   --    The id belonging to given Node_Attribute
   --    Invalid_Attribute_Id if given Node_Class does not exist
   --
   --  Raises:
   --    Node_Class_Does_Not_Exist     if given Node_Class does not exist
   --    Node_Attribute_Does_Not_Exist if given Node_Attribute does not exist
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Node_Class_Id belonging to given Node_Class_Name
   --  Parameters:
   --    Node_Class_Name in ("Id", "Id_Set", ...)
   --    defined in IML_refelection - Abstract_Class.Name
   --  Raises:
   --    Node_Class_Does_Not_Exist, if Does_Node_Class_Exist() = False
   function Convert_Node_Class_Name_To_Id
      (Node_Class_Name : in String)
      return Node_Class_Id;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Edge_Class_Does_Not_Exist, if Does_Edge_Class_Exist() = False
   function Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
      (Node_Class     : in Node_Class_Id;
       Node_Attribute : in Node_Attribute_Id)
      return Edge_Class_Id;

   -----------------------------------
   -- Inspectors on Class-Relations --
   -----------------------------------

   ---------------------------------------------------------------------------
   --  Gets the class belonging to given Attribute
   function Get_Node_Attribute_Class_Id
      (Node_Attribute : in Node_Attribute_Id)
      return Node_Attribute_Class_Id;


   --------------------------------
   -- Inspectors                 --
   --  On the nodes of the graph --
   --------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    "Type of Node"
   function Get_Class_Of_Node
      (Node : in Node_Id)
      return Node_Class_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All incoming edges of given node
   function Get_Incoming_Edges
      (Node : in Node_Id)
      return Edge_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All outgoing edges of given node
   function Get_Outgoing_Edges
      (Node : in Node_Id)
      return Edge_Id_Set;

   --------------------------------
   -- Inspectors                 --
   --  On the edges of the graph --
   --------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    "Type of Edge"
   function Get_Class_Of_Edge
      (Edge : in Edge_Id)
      return Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Source_Node
      (Edge : in Edge_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   function Get_Target_Node
      (Edge : in Edge_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    <Attribute_Name>.<Number in list/set> if given Edge was created
   --      from an list/set
   --    <Attribute_Name> else
   function Get_Edge_Tag
      (Edge : Edge_Id)
      return String;

   ---------------------------------
   -- Inspectors                  --
   --  On the struct of the graph --
   ---------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    Root-Node of the IML-Graph
   function Get_Root_Node
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All nodes in the IML-Graph
   function Get_All_Nodes
      return Node_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All known node_class Ids
   function Get_All_Node_Class_Ids return Node_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All known edge_class Ids
   function Get_All_Edge_Class_Ids return Edge_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Returns:
   --    All Edge_Class_Ids having given Node_Class (as Source)
   --    Empty_Set if there is none.
   function Get_All_Edge_Class_Ids_For_Node_Class
      (Node_Class : in Node_Class_Id)
      return Edge_Class_Id_Set;

   ---------------------------------------------------------------------------
   --  Used to determine edges having a certain attribute
   --    It is not depended of a certain Source_Node_Class
   --
   --  Returns:
   --    All Edge_Class_Ids which contains given Attribute
   --    Empty_Set if there are none.
   --  Extendable:
   --    s/Node_Attribute/Edge_Attribute/
   function Get_All_Edge_Class_Ids_For_Node_Attribute
      (Node_Attribute_Name : in String)
      return Edge_Class_Id_Set;

   ----------------------------------------------------
   --  Routines for an immutable node id             --
   --  Idea is like Integer'Value and Integer'Image  --
   ----------------------------------------------------

   ---------------------------------------------------------------------------
   --  The generated string is unique and invariant over multiple runs of
   --    GIANT
   --
   --  Maybe u have to use
   --    "for Node_Id'Image use Node_Id_Image"
   --
   --  Returns:
   --    String-representation of given Node_Id
   --
   --  Raises:
   --    Storables.Unknown_Node if something is wrong in the IML-Graph
   function Node_Id_Image
     (Node : in Node_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Converts given string containing a Node_Id to a Node_Id
   --
   --  Raises:
   --    Node_Does_Not_Exist if given node_id_string does not match an
   --    existing node
   function Node_Id_Value
     (Node : in String)
     return Node_Id;

   ---------------------------------------------------------------------------
   --  inconsitent to Integer'Value and 'Image, but consistent to other
   --  routines like Node_Class_Id
   --
   --  Returns
   --    True if given Node_Id exists (i.e. can be converted to a Node_Id)
   function Does_Node_Id_Exist
     (Node : in String)
     return Boolean;

   ---------------------------
   -- Inspectors            --
   --  On the data of nodes --
   ---------------------------

   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_Boolean
   function Get_Node_Attribute_Boolean_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_Natural
   function Get_Node_Attribute_Natural_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Returns:
   --    Invalid_Node_Id if Attribute doesn't show an valid node
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_Node_Id
   --    Node_Does_Not_Exist
   --      if edge does not point to a valid node-id
   function Get_Node_Attribute_Node_Id_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_Node_Id_List
   function Get_Node_Attribute_Attribute_Node_Id_List_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id_List;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_Node_Id_Set
   function Get_Node_Attribute_Node_Id_Set_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Node_Id_Set;


   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_String
   --  function Get_Node_Attribute_String_Value
   --     (Node      : in     Node_Id;
   --      Attribute : in     Node_Attribute_Id)
   --     return String;

   ---------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_String_List
--   function Get_Node_Attribute_String_Lists_Value
--      (Node      : in     Node_Id;
--       Attribute : in     Node_Attribute_Id)
--      return String_Lists.Lists;

   ---------------------------------------------------------------------------
   --  SLoc is split up into seperate parts, since GSL doesn't know type
   --    Sloc
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Slocs.Get_Line()-Wrapper
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_SLoc
   function Get_Node_Attribute_SLoc_Line_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Column()-Wrapper
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_SLoc
   function Get_Node_Attribute_SLoc_Column_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return Natural;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Path()-Wrapper
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_SLoc
   function Get_Node_Attribute_SLoc_Path_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;

   ---------------------------------------------------------------------------
   --  Slocs.Get_Filename()-Wrapper
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_SLoc
   function Get_Node_Attribute_SLoc_Filename_Value
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;


   ---------------------------------------------------------------------------
   --  Returns:
   --    Value of the attribute converted to a String
   --    Invalid_Attribute_Value_String if
   --      * Attribute contains an invalid value
   --      * Conversion to string is not implemented for given Attribute_Id
   function Get_Node_Attribute_Value_As_String
      (Node      : in     Node_Id;
       Attribute : in     Node_Attribute_Id)
      return String;


   ---------------
   -- Iterators --
   ---------------

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
      (Node     : in     Node_Id)
      return Node_Attribute_Iterator;

   ---------------------------------------------------------------------------
   --  analogue to Bauhaus::Lists
   function More
     (Iterator : in Node_Attribute_Iterator)
     return Boolean;

   ---------------------------------------------------------------------------
   --  analogue to Bauhaus::Lists
   --  Raises:
   --    dont know yet - TBD!
   procedure Next
     (Iterator : in out Node_Attribute_Iterator;
      Info     :    out Node_Attribute_Id);

   -------------
   -- Hashing --
   -------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    Hash of the loaded IML-Graph
   function Get_Graph_Hash return Integer;

   ---------------------------------------------------------------------------
   function Hash_Node_Id (Key : in Node_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Edge_Id (Key : in Edge_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Node_Class_Id (Key : in Node_Class_Id) return Integer;

   ---------------------------------------------------------------------------
   function Hash_Edge_Class_Id (Key : in Edge_Class_Id) return Integer;

private

   ---------------------------------------------------------------------------
   --  Needed for Node_Record, used to store incoming and outgoing edges
   type Edge_Id_Array is array (Positive range <>) of Edge_Id;

   ---------------------------------------------------------------------------
   --  Represents a single node of the IML_Graph
   type Node_Record
     (Number_Of_Incoming_Edges : Natural;
      Number_Of_Outgoing_Edges : Natural) is
   limited record
      IML_Node       : Storables.Storable;
      Incoming_Edges : Edge_Id_Array (1 .. Number_Of_Incoming_Edges);
      Outgoing_Edges : Edge_Id_Array (1 .. Number_Of_Outgoing_Edges);
   end record;

   ---------------------------------------------------------------------------
   --  Represents an edge of the IML_Graph
   type Edge_Record is
     record
        Source_Node                   : Node_Id;
        Target_Node                   : Node_Id;

        --  Indicates from which attribute this edge was generated from
        Attribute                     : Node_Attribute_Id;

        --   =0 : n/a
        --  /=0 : element# of list/set where attribute was included
        Attribute_Element_Number     : Natural;
     end record;

   ---------------------------------------------------------------------------
   --  an unique edgeclass
   type Edge_Class is
     limited record
        Source_Node_Class     : Node_Class_Id;
        Source_Node_Attribute : Node_Attribute_Id;
     end record;

   Invalid_Node_Id                : constant Node_Id := null;
   Invalid_Attribute_Value_String : constant String  := "*INVALID*";

   ---------------------------------------------------------------------------
   type Node_Attribute_Iterator is record
      CurrentIndex : Integer;

      --  The class of the node on which the iterator iterates on
      Class        : IML_Reflection.Class_ID;
   end record;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Attribute_Id;
       Right : Node_Attribute_Id)
      return Boolean;

   ----------------------------------------------------------------------------
   --  Raises:
   --    Wrong_Attribute_Type
   --      if Get_Node_Attribute_Class_Id(Attribute) /= Class_SLoc
   function Get_Node_Attribute_SLoc_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return SLocs.Sloc;

end Giant.Graph_Lib;
