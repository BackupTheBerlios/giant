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
--  $RCSfile: giant-graph_lib.adb,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/05/28 12:17:02 $
--

--  from Bauhaus
with Constant_Ptr_Hashs;
with Ptr_Normal_Hashs;
with Hashed_Mappings;
with SLocs;
with Storables;

pragma Elaborate_All (Constant_Ptr_Hashs);
pragma Elaborate_All (Ptr_Normal_Hashs);
pragma Elaborate_All (Hashed_Mappings);

package body Giant.Graph_Lib is

   Invalid_Node_Attribute_Id : constant Node_Attribute_Id := null;

   --------------------------------------
   --  Hashing for Node_Attribute_Ids  --
   --------------------------------------

   ---------------------------------------------------------------------------
   --  How large should be the hashtable
   --    Default of ptr_hashs: 17
   Node_Attribute_Id_Hash_Range_Size : constant := 29;

   ---------------------------------------------------------------------------
   package Node_Attribute_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => Node_Attribute,
      T_Ptr      => Node_Attribute_Id,
      Range_Size => Node_Attribute_Id_Hash_Range_Size);

   ---------------------------------------------------------------------------
   package Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => Node_Attribute_Id,
      Value_Type => Edge_Class_Id,
      Hash       => Node_Attribute_Id_Hash_Functions.Integer_Hash);

   ----------------------------------
   --  Hashing for Node_Class_Ids  --
   ----------------------------------

   ---------------------------------------------------------------------------
   --  How large should be the hashtable
   --    Default of ptr_hashs: 17
   Node_Class_Id_Hash_Range_Size : constant := 29;

   ---------------------------------------------------------------------------
   package Node_Class_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => Node_Class,
      T_Ptr      => Node_Class_Id,
      Range_Size => Node_Class_Id_Hash_Range_Size);

   ---------------------------------------------------------------------------
   type Node_Class_Id_Hash_Data is record
      --  Hastable hasing Node_Attribute_Ids to Edge_Class_Ids
      Node_Attribute_Id_Mapping :
        Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Mapping;

      --  further contents will surely come
   end record;

   ---------------------------------------------------------------------------
   type Node_Class_Id_Hash_Data_Access is access Node_Class_Id_Hash_Data;

   ---------------------------------------------------------------------------
   package Node_Class_Id_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => Node_Class_Id,
      Value_Type => Node_Class_Id_Hash_Data_Access,
      Hash       => Node_Class_Id_Hash_Functions.Integer_Hash);

   function "<"
     (Left  : Node_Id;
      Right : Node_Id)
     return Boolean
   is
   begin
      --  < cannot be used, since that would case an infinite recursion
      --  therefore, we have to trick a bit :)
      return Storables.Less (Left.Iml_Node, Right.Iml_Node);
   end "<";

   ---------------------------------------------------------------------------
   Node_Class_Id_Mapping : Node_Class_Id_Hashed_Mappings.Mapping;

   ---------------------------------------------------------------------------
   --  Compares using IML_Reflection-Data
   function "<"
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return (Left.all.all.Name < Right.all.all.Name);
   end "<";

   ---------------------------------------------------------------------------
   --  Maybe this routine has to be rewritten, since it contains a partial
   --    ordering and not an absolute one. The latter could be achieved using
   --    the pointer's value in the memory, but I currently do not know how
   --    to do that in a clean way
   function "<"
     (Left  : Edge_Id;
      Right : Edge_Id)
      return Boolean
   is
   begin
      return
        (Left.Source_Node    < Right.Source_Node) or
        (Left.Target_Node    < Right.Target_Node) or
        (Left.Node_Attribute < Right.Node_Attribute);
   end "<";

   ---------------------------------------------------------------------------
   --  Maybe this routine has to be rewritten, since it contains a partial
   --    ordering and not an absolute one. The latter could be achieved using
   --    the pointer's value in the memory, but I currently do not know how
   --    to do that in a clean way
   function "<"
     (Left  : Edge_Class_Id;
      Right : Edge_Class_Id)
      return Boolean
   is
   begin
      return
        (Left.Source_Node_Class     < Right.Source_Node_Class) or
        (Left.Source_Node_Attribute < Right.Source_Node_Attribute);
   end "<";

   function "<"
      (Left  : Node_Attribute_Id;
       Right : Node_Attribute_Id)
      return Boolean
   is
   begin
      return (Left.all.all.Name < Right.all.all.Name);
   end "<";

   function "="
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return (Left.all.all.Name = Right.all.all.Name);
   end "=";

   function Convert_Node_Attribute_Id_To_Name
     (Node_Attribute : in Node_Attribute_Id)
      return String
   is
   begin
      return Node_Attribute.all.all.Name;
   end Convert_Node_Attribute_Id_To_Name;

   ---------------------------------------------------------------------------
   --  Performance can be increased if no linear search, but a hasing is used
   --    (In create() the hasing should be instantiated)
   --
   --  Case-senstive search
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id
   is
      IML_Class      : IML_Reflection.Class_ID;
      Node_Attrib    : Node_Attribute_Id := Invalid_Node_Attribute_Id;
   begin
      --  initialise IML_Class
      declare
         Node_Class : Node_Class_Id;
      begin
         Node_Class := Convert_Node_Class_Name_To_Id (Node_Class_Name);
         IML_Class  := IML_Reflection.Class_ID (Node_Class.all);
      end;

      declare
         I         : Integer;
      begin
         I := IML_Class.Fields'First;
         while
           (I <= IML_Class.Fields'Last) and then
           (IML_Class.Fields (I).Name /= Node_Attribute_Name)
         loop
            I := I + 1;
         end loop;
         if I <= IML_Class.Fields'Last then
            --  hash back from IML_Field_Id to Node_Attribute_Id
            null;
         end if;
      end;

      if Node_Attrib = Invalid_Node_Attribute_Id then
         raise Node_Attribute_Does_Not_Exist;
      end if;

      return Node_Attrib;
   end Convert_Node_Attribute_Name_To_Id;

   function Convert_Node_Class_Name_To_Id
     (Node_Class_Name : in String)
      return Node_Class_Id
   is
   begin
      --  where the XXX is IML_Classes.Get_all_Classes
      --  I think, it was never implemented O:)
      return Convert_Node_Class_Name_To_Id (Node_Class_Name);
   end Convert_Node_Class_Name_To_Id;

   function Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
     (Node_Class     : in Node_Class_Id;
      Node_Attribute : in Node_Attribute_Id)
      return Edge_Class_Id
   is
      Node_Class_Data : Node_Class_Id_Hash_Data_Access;
      Edge_Class      : Edge_Class_Id;
   begin
      begin
         Node_Class_Data := Node_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Id_Mapping, Node_Class);
      exception
         when Node_Class_Id_Hashed_Mappings.Uninitialized_Mapping =>
            raise Program_Error;
         when Node_Class_Id_Hashed_Mappings.Not_Bound =>
            raise Node_Class_Does_Not_Exist;
            --  return Invalid_Edge_Class_Id;
      end;

      begin
         Edge_Class := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Data.Node_Attribute_Id_Mapping, Node_Attribute);
      exception
         when Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Uninitialized_Mapping =>
            raise Program_Error;
         when Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Not_Bound =>
            raise Node_Attribute_Does_Not_Exist;
      end;

      return Edge_Class;
   end Convert_Node_Class_Node_Attribute_To_Edge_Class_Id;

   procedure Create (Path_To_IML_File : in String) is
   begin
      null;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy is
   begin
      null;
   end Destroy;

   ---------------------------
   -- Does_Edge_Class_Exist --
   ---------------------------

   function Does_Edge_Class_Exist
     (Node_Class     : in Node_Class_Id;
      Node_Attribute : in Node_Attribute_Id)
      return Boolean
   is
   begin
      return Does_Edge_Class_Exist (Node_Class, Node_Attribute);
   end Does_Edge_Class_Exist;

   -------------------------------
   -- Does_Node_Attribute_Exist --
   -------------------------------

   function Does_Node_Attribute_Exist
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
      return Boolean
   is
   begin
      return Does_Node_Attribute_Exist (Node_Class_Name, Node_Attribute_Name);
   end Does_Node_Attribute_Exist;

   ---------------------------
   -- Does_Node_Class_Exist --
   ---------------------------

   function Does_Node_Class_Exist
     (Node_Class_Name : in String)
      return Boolean
   is
   begin
      return Does_Node_Class_Exist (Node_Class_Name);
   end Does_Node_Class_Exist;

   ----------------------------
   -- Get_All_Edge_Class_Ids --
   ----------------------------

   function Get_All_Edge_Class_Ids return Edge_Class_Id_Set is
   begin
      return Get_All_Edge_Class_Ids;
   end Get_All_Edge_Class_Ids;

   function Node_Id_Image
     (Node : in Node_Id)
     return String
   is
   begin
      return Node_Id_Image (Node);
   end Node_Id_Image;

   function Node_Id_Value
     (Node : in String)
     return Node_Id
   is
   begin
      return Node_Id_Value (Node);
   end Node_Id_Value;

   function Does_Node_Id_Exist
     (Node : in String)
     return Boolean
   is
   begin
      --  TBD
      return False;
   end Does_Node_Id_Exist;

   -----------------------------------------------
   -- Get_All_Edge_Class_Ids_For_Node_Attribute --
   -----------------------------------------------

   function Get_All_Edge_Class_Ids_For_Node_Attribute
     (Node_Attribute_Name : in String)
      return Edge_Class_Id_Set
   is
   begin
      return Get_All_Edge_Class_Ids_For_Node_Attribute (Node_Attribute_Name);
   end Get_All_Edge_Class_Ids_For_Node_Attribute;

   -------------------------------------------
   -- Get_All_Edge_Class_Ids_For_Node_Class --
   -------------------------------------------

   function Get_All_Edge_Class_Ids_For_Node_Class
     (Node_Class : in Node_Class_Id)
      return Edge_Class_Id_Set
   is
   begin
      return Get_All_Edge_Class_Ids_For_Node_Class (Node_Class);
   end Get_All_Edge_Class_Ids_For_Node_Class;

   ----------------------------
   -- Get_All_Node_Class_Ids --
   ----------------------------

   function Get_All_Node_Class_Ids return Node_Class_Id_Set is
   begin
      return Get_All_Node_Class_Ids;
   end Get_All_Node_Class_Ids;

   -------------------
   -- Get_All_Nodes --
   -------------------

   function Get_All_Nodes
      return Node_Id_Set
   is
   begin
      return Get_All_Nodes;
   end Get_All_Nodes;

   -----------------------
   -- Get_Class_Of_Edge --
   -----------------------

   function Get_Class_Of_Edge
     (Node : in Edge_Id)
      return Edge_Class_Id
   is
   begin
      return Get_Class_Of_Edge (Node);
   end Get_Class_Of_Edge;

   -----------------------
   -- Get_Class_Of_Node --
   -----------------------

   function Get_Class_Of_Node
     (Node : in Node_Id)
      return Node_Class_Id
   is
   begin
      return Get_Class_Of_Node (Node);
   end Get_Class_Of_Node;

   ------------------
   -- Get_Edge_Tag --
   ------------------

   function Get_Edge_Tag
     (Edge : Edge_Id)
      return String
   is
   begin
      return Get_Edge_Tag (Edge);
   end Get_Edge_Tag;

   ------------------------
   -- Get_Incoming_Edges --
   ------------------------

   function Get_Incoming_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Get_Incoming_Edges (Node);
   end Get_Incoming_Edges;

   -----------------------------------------------------
   -- Get_Node_Attribute_Attribute_Node_Id_List_Value --
   -----------------------------------------------------

   function Get_Node_Attribute_Attribute_Node_Id_List_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_List
   is
   begin
      return Get_Node_Attribute_Attribute_Node_Id_List_Value (Node, Attribute);
   end Get_Node_Attribute_Attribute_Node_Id_List_Value;

   function Get_Node_Attribute_Boolean_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Boolean
   is
   begin
      return Get_Node_Attribute_Boolean_Value (Node, Attribute);
   end Get_Node_Attribute_Boolean_Value;

   ---------------------------------
   -- Get_Node_Attribute_Class_Id --
   ---------------------------------

   function Get_Node_Attribute_Class_Id
     (Node_Attribute : in Node_Attribute_Id)
      return Node_Attribute_Class_Id
   is
   begin
      return Get_Node_Attribute_Class_Id (Node_Attribute);
   end Get_Node_Attribute_Class_Id;

   --------------------------------------
   -- Get_Node_Attribute_Natural_Value --
   --------------------------------------

   function Get_Node_Attribute_Natural_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return Get_Node_Attribute_Natural_Value (Node, Attribute);
   end Get_Node_Attribute_Natural_Value;

   ------------------------------------------
   -- Get_Node_Attribute_Node_Id_Set_Value --
   ------------------------------------------

   function Get_Node_Attribute_Node_Id_Set_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_Set
   is
   begin
      return Get_Node_Attribute_Node_Id_Set_Value (Node, Attribute);
   end Get_Node_Attribute_Node_Id_Set_Value;

   --------------------------------------
   -- Get_Node_Attribute_Node_Id_Value --
   --------------------------------------

   function Get_Node_Attribute_Node_Id_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id
   is
   begin
      return Get_Node_Attribute_Node_Id_Value (Node, Attribute);
   end Get_Node_Attribute_Node_Id_Value;

   ------------------------------------------
   -- Get_Node_Attribute_SLoc_Column_Value --
   ------------------------------------------

   function Get_Node_Attribute_SLoc_Column_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return Get_Node_Attribute_SLoc_Column_Value (Node, Attribute);
   end Get_Node_Attribute_SLoc_Column_Value;

   --------------------------------------------
   -- Get_Node_Attribute_SLoc_Filename_Value --
   --------------------------------------------

   function Get_Node_Attribute_SLoc_Filename_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return Get_Node_Attribute_SLoc_Filename_Value (Node, Attribute);
   end Get_Node_Attribute_SLoc_Filename_Value;

   ----------------------------------------
   -- Get_Node_Attribute_SLoc_Line_Value --
   ----------------------------------------

   function Get_Node_Attribute_SLoc_Line_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return Get_Node_Attribute_SLoc_Line_Value (Node, Attribute);
   end Get_Node_Attribute_SLoc_Line_Value;

   ----------------------------------------
   -- Get_Node_Attribute_SLoc_Path_Value --
   ----------------------------------------

   function Get_Node_Attribute_SLoc_Path_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      return Get_Node_Attribute_SLoc_Path_Value (Node, Attribute);
   end Get_Node_Attribute_SLoc_Path_Value;

   -------------------------------------
   -- Get_Node_Attribute_String_Value --
   -------------------------------------

   function Get_Node_Attribute_String_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      return Get_Node_Attribute_String_Value (Node, Attribute);
   end Get_Node_Attribute_String_Value;

   ----------------------------------------
   -- Get_Node_Attribute_Value_As_String --
   ----------------------------------------

   function Get_Node_Attribute_Value_As_String
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      case Get_Node_Attribute_Class_Id (Attribute) is
         when Class_SLoc =>
            declare
               SLoc_StoredInIML : IML_Reflection.SLoc_Field
                 := IML_Reflection.SLoc_Field (Attribute.all.all);
               SLoc             : SLocs.SLoc;
            begin
               SLoc := SLoc_StoredInIML.Get_Value (Node.Iml_Node);
               return SLocs.Plain_Image (SLoc);
            end;
         when others =>
            return "";
      end case;
   end Get_Node_Attribute_Value_As_String;

   ------------------------
   -- Get_Outgoing_Edges --
   ------------------------

   function Get_Outgoing_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Get_Outgoing_Edges (Node);
   end Get_Outgoing_Edges;

   -------------------
   -- Get_Root_Node --
   -------------------

   function Get_Root_Node
      return Node_Id
   is
   begin
      return Get_Root_Node;
   end Get_Root_Node;

   ---------------------
   -- Get_Source_Node --
   ---------------------

   function Get_Source_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Get_Source_Node (Edge);
   end Get_Source_Node;

   ---------------------
   -- Get_Target_Node --
   ---------------------

   function Get_Target_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Get_Target_Node (Edge);
   end Get_Target_Node;

   ------------------------
   -- Has_Node_Attribute --
   ------------------------

   function Has_Node_Attribute
      (Node_Class_Name     : in String;
       Node_Attribute_Name : in String)
      return Boolean
   is
   begin
      return Has_Node_Attribute (Node_Class_Name, Node_Attribute_Name);
   end Has_Node_Attribute;

   ------------------------
   -- Hash_Edge_Class_Id --
   ------------------------

   function Hash_Edge_Class_Id (Key : in Edge_Class_Id) return Integer is
   begin
      return Hash_Edge_Class_Id (Key);
   end Hash_Edge_Class_Id;

   ------------------
   -- Hash_Edge_Id --
   ------------------

   function Hash_Edge_Id (Key : in Edge_Id) return Integer is
   begin
      return Hash_Edge_Id (Key);
   end Hash_Edge_Id;

   ------------------------
   -- Hash_Node_Class_Id --
   ------------------------

   function Hash_Node_Class_Id (Key : in Node_Class_Id) return Integer is
   begin
      return Hash_Node_Class_Id (Key);
   end Hash_Node_Class_Id;

   function Hash_Node_Id (Key : in Node_Id) return Integer is
   begin
      return Hash_Node_Id (Key);
   end Hash_Node_Id;

   procedure Make_Attribute_Iterator
     (Node     : in     Node_Id;
      Iterator :    out Node_Attribute_Iterator)
   is
   begin
      null;
   end Make_Attribute_Iterator;

   procedure Destroy_Attribute_Iterator
      (Node     : in     Node_Id;
       Iterator : in out Node_Attribute_Iterator)
   is
   begin
      null;
   end Destroy_Attribute_Iterator;

end Giant.Graph_Lib;

