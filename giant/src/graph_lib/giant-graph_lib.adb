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
--  $RCSfile: giant-graph_lib.adb,v $, $Revision: 1.5 $
--  $Author: koppor $
--  $Date: 2003/06/09 22:08:53 $

--  from ADA
with Ada.Unchecked_Deallocation;

--  from Bauhaus
with Hashed_Mappings;
with Ptr_Hashs;
with SLocs;
with Storables;
with IML_Graphs;
with IML.IO;
with IML_Node_IDs;
with IML_Reflection;
with IML_Roots;

--  from Giant
with Giant.Constant_Ptr_Hashs;
with Giant.Ptr_Normal_Hashs;
with Giant.Lists;

with Giant.Logger;


pragma Elaborate_All (Giant.Constant_Ptr_Hashs);
pragma Elaborate_All (Giant.Ptr_Normal_Hashs);
pragma Elaborate_All (Hashed_Mappings);
pragma Elaborate_All (Ptr_Hashs);


package body Giant.Graph_Lib is

   package My_Logger is new Logger("giant.graph_lib");

   Invalid_Node_Attribute_Id : constant Node_Attribute_Id := null;

   --  reference to loaded IML-Graph
   --  Created in "Create"
   --  Destroyed in "Destroy"
   IML_Graph : IML_Graphs.IML_Graph;


   --------------------------------------
   --  Hashing for Node_Attribute_Ids  --
   --------------------------------------

   ---------------------------------------------------------------------------
   --  Hashtable-size
   --    Default of ptr_hashs: 17
   Node_Attribute_Id_Hash_Range_Size : constant := 29;

   ---------------------------------------------------------------------------
   package Node_Attribute_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => IML_Reflection.Field'Class,
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
   --  Hashtable-size
   --    Default of ptr_hashs: 17
   Node_Class_Id_Hash_Range_Size : constant := 29;

   ---------------------------------------------------------------------------
   package Node_Class_Id_Hash_Functions is
      new Constant_Ptr_Hashs
     (T          => IML_Reflection.Abstract_Class'Class,
      T_Ptr      => Node_Class_Id,
      Range_Size => Node_Class_Id_Hash_Range_Size);

   ---------------------------------------------------------------------------
   type Node_Class_Id_Hash_Data is record
      --  Hashtable hasing Node_Attribute_Ids to Edge_Class_Ids
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

   ---------------------------------------------------------------------------
   Node_Class_Id_Mapping : Node_Class_Id_Hashed_Mappings.Mapping;

   ----------------------------------
   --  Hashing for IML_Node_IDs    --
   --    needed for Node_Id_Value  --
   ----------------------------------

   ---------------------------------------------------------------------------
   package IML_Node_ID_Hashed_Mappings is
      new Hashed_Mappings
     (Key_Type   => IML_Node_IDs.Node_ID,
      Value_Type => Node_Id,
      Hash       => IML_Node_IDs.Hash);

   IML_Node_ID_Mapping : IML_Node_ID_Hashed_Mappings.Mapping;

   ---------------------
   --  Miscellaneous  --
   ---------------------

   ---------------------------------------------------------------------------
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
   --  Compares using IML_Reflection-Data
   function "<"
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return (Left.Name < Right.Name);
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
        (Left.Attribute      < Right.Attribute);
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
      return (Left.Name < Right.Name);
   end "<";

   function "="
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return (Left.Name = Right.Name);
   end "=";

   function Convert_Node_Attribute_Id_To_Name
     (Node_Attribute : in Node_Attribute_Id)
      return String
   is
   begin
      return Node_Attribute.Name;
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
         IML_Class  := Node_Class;
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

      if IML_Reflection."=" (Node_Attrib, Invalid_Node_Attribute_Id) then
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

   ----------------------------------------------------------
   -- Create the internal representation of the IML-Graph  --
   ----------------------------------------------------------
   procedure Create (Path_To_IML_File : in String) is

      -----------------------------------------------------------
      --  Data-Structore for temporary graph used for loading  --
      -----------------------------------------------------------

      package Load_Nodes is

         type Node_Record;
         type Node_Access is access all Node_Record;

         type Edge_Record is record
            Source     : Node_Access;
            Target     : Node_Access;

            --  like in "outer" Edge_Record
            Attribute                : Node_Attribute_Id;
            Attribute_Element_Number : Natural;
         end record;

         type Edge_Access is access Edge_Record;

         package Edge_Lists is new Giant.Lists
           (ItemType => Edge_Access);

         type Node_Record is record
            Edges_In      : Edge_Lists.List;
            Edges_Out     : Edge_Lists.List;
            IML_Node      : Storables.Storable;

            --  Used at conversion from temporary structure to
            --  internal structure
            Internal_Node : Node_Id;
         end record;

         --  Creates an edge in the internal structure
         procedure Create_Edge
           (Edge_Source : in Node_Access;
            Edge_Target : in Node_Access;
            Attribute   : in Node_Attribute_Id;
            Attribute_Element_Number : in Natural);

         --  Creates a node in the internal structure
         --  they are NOT collected in an internal list
         --  The caller has to handle the destroyage
         function Create_Node
           (IMLNode : Storables.Storable)
           return Node_Access;

         --  Destroys the node and all outgoing edges
         --  ! The edges pointing to this node are /not/ destroyed
         procedure Destroy_Node (NodeToDestroy : in out Node_Access);

         package Node_Queues is new Giant.Lists (ItemType => Node_Access);
         subtype Node_Queue is Node_Queues.List;

      end Load_Nodes;

      package body Load_Nodes is

         procedure Create_Edge
           (Edge_Source : in Node_Access;
            Edge_Target : in Node_Access;
            Attribute   : in Node_Attribute_Id;
            Attribute_Element_Number : in Natural) is

            Edge : Edge_Access := new Edge_Record;
         begin
            Edge.Source := Edge_Source;
            Edge.Target := Edge_Target;
            Edge.Attribute := Attribute;
            Edge.Attribute_Element_Number := Attribute_Element_Number;

            Edge_Lists.Attach (Edge_Target.Edges_In, Edge);
            Edge_Lists.Attach (Edge_Source.Edges_Out, Edge);
         end Create_Edge;

         function Create_Node
           (IMLNode : Storables.Storable)
           return Node_Access is
            NewNode : Node_Access := new Node_Record;
         begin
            NewNode.IML_Node  := IMLNode;
            NewNode.Edges_In  := Edge_Lists.Create;
            NewNode.Edges_Out := Edge_Lists.Create;
            return NewNode;
         end Create_Node;

         procedure Destroy_Node (NodeToDestroy : in out Node_Access) is

            procedure Free_Node is new Ada.Unchecked_Deallocation
              (Node_Record, Node_Access);

            procedure Free_Edge is new Ada.Unchecked_Deallocation
              (Edge_Record, Edge_Access);

            procedure DestroyDeep_Edges is new Edge_Lists.DestroyDeep
              (Dispose => Free_Edge);

         begin
            DestroyDeep_Edges (NodeToDestroy.Edges_Out);
            Free_Node (NodeToDestroy);
         end Destroy_Node;

      end Load_Nodes;


      -----------------------------
      --  Hashing for IML_Nodes  --
      -----------------------------

      --  Oriented on vis_test.iml_graph_loader
      package IML_Node_Mapper is
         procedure Create;
         procedure Destroy;

         ---------------------------------------------------------------------
         --  Gets an node out of the hashtable
         --  Creates it, if it doesn't exist
         --
         --  Parameters:
         --    Created': True,  if a mapping was created
         --              False, if a mapping already existed
         procedure Get
           (Iml_Node : in     Storables.Storable;
            Node     :    out Load_Nodes.Node_Access;
            Created  :    out Boolean);
      end IML_Node_Mapper;

      package body IML_Node_Mapper is

         package Storable_Hashs is new Ptr_Hashs
           (T     => Storables.Storable_Class,
            T_Ptr => Storables.Storable);

         package Mapping_Iml_LoadNodes is new Hashed_Mappings
           (Key_Type   => Storables.Storable,
            Hash       => Storable_Hashs.Integer_Hash,
            Value_Type => Load_Nodes.Node_Access);

         Mapping : Mapping_Iml_LoadNodes.Mapping;

         procedure Create is
         begin
            Mapping := Mapping_Iml_LoadNodes.Create;
         end Create;

         procedure Destroy is
         begin
            Mapping_Iml_LoadNodes.Destroy (Mapping);
         end Destroy;

         procedure Get
           (Iml_Node : in     Storables.Storable;
            Node     :    out Load_Nodes.Node_Access;
            Created  :    out Boolean) is
         begin
            if Mapping_Iml_LoadNodes.Is_Bound (Mapping, Iml_Node) then
               Node := Mapping_Iml_LoadNodes.Fetch (Mapping, Iml_Node);
               Created := False;
            else
               Node := Load_Nodes.Create_Node (Iml_Node);
               Mapping_Iml_LoadNodes.Bind (Mapping, Iml_Node, Node);
               Created := True;
            end if;
         end Get;

      end IML_Node_Mapper;

      ------------------------------------------------------------------------
      --  Parameters:
      --    Queue' : All generated nodes will be stored there
      procedure ConvertIMLGraphToTempStructure
        (Queue : out Load_Nodes.Node_Queue)
      is

         procedure ProcessQueue is

            --  Does all necessary things, if there's an edge from
            --    a Node to an IML_Node
            procedure Process_Edge
              (Source_Node : in Load_Nodes.Node_Access;
               Target      : in Storables.Storable;
               Attribute   : in Node_Attribute_Id;
               Attribute_Element_Number : in Natural) is

               Created     : Boolean;
               Target_Node : Load_Nodes.Node_Access;
            begin
               IML_Node_Mapper.Get (Target, Target_Node,  Created);

               if Created then
                  Load_Nodes.Node_Queues.Attach (Queue, Target_Node);
               end if;

               Load_Nodes.Create_Edge
                 (Source_Node,
                  Target_Node,
                  Attribute,
                  Attribute_Element_Number);
            end Process_Edge;

            Node  : Load_Nodes.Node_Access;

            procedure Process_Attribute
              (IML_Node  : in Storables.Storable;
               Attribute : in IML_Reflection.Field_ID) is

               Target : Storables.Storable;
            begin
               if Attribute.all in IML_Reflection.Edge_Field'Class then
                  declare
                     IML_Edge : IML_Reflection.Edge_Field
                       := IML_Reflection.Edge_Field (Attribute.all);
                  begin
                     Target   := IML_Edge.Get_Target
                       (IML_Roots.IML_Root (IML_Node));
                     if Storables."/=" (Target, null) then
                        Process_Edge (Node, Target, Attribute, 0);
                     else
                        My_Logger.Info ("Edge_Field with null target ignored");
                     end if;
                  end;
               elsif Attribute.all in IML_Reflection.List_Field'Class then
                  declare
                     IML_List  : IML_Reflection.List_Field
                       := IML_Reflection.List_Field (Attribute.all);
                     Iter      : IML_Reflection.List_Iterator;
                     Target    : Storables.Storable;
                     I         : Natural := 0;
                  begin
                     Iter     := IML_List.Make_Iterator (IML_Node);

                     while IML_Reflection.More (Iter) loop
                        IML_Reflection.Next (Iter, Target);
                        I := I+1;

                        Process_Edge (Node, Target, Attribute, I);
                     end loop;
                  end;
               elsif Attribute.all in IML_Reflection.Set_Field'Class then
                  declare
                     IML_Set : IML_Reflection.Set_Field
                       := IML_Reflection.Set_Field (Attribute.all);
                     Iter    : IML_Reflection.Set_Iterator;
                     Target  : Storables.Storable;
                     I       : Natural := 0;
                  begin
                     Iter    := IML_Set.Make_Iterator (IML_Node);

                     while IML_Reflection.More (Iter) loop
                        IML_Reflection.Next (ITer, Target);
                        I := I+1;

                        Process_Edge (Node, Target, Attribute, I);
                     end loop;
                  end;
               elsif
                 (Attribute.all in
                  IML_Reflection.Identifier_Field'Class) or
                 (Attribute.all in
                  IML_Reflection.Builtin_Field'Class) then
                  null;
               else
                  My_Logger.Error ("Unknown IML_Reflection.Field");
               end if;
            end Process_Attribute;

            Iter      : Load_Nodes.Node_Queues.ListIter;

            Class     : IML_Reflection.Class_ID;
            Attribute : IML_Reflection.Field_ID;

         begin
            Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);

            while Load_Nodes.Node_Queues.More (Iter) loop
               Node := Load_Nodes.Node_Queues.CurrentElement (Iter);

               if Node.IML_Node.all in IML_Roots.IML_Root_Class'Class then
                  --  we process only nodes below IML_Root and no other
                  --  storables

                  Class := IML_Roots.Get_Class_ID
                    (IML_Roots.IML_Root (Node.IML_Node));

                  for I in Class.Fields'Range loop
                     Attribute := Class.Fields (I);
                     Process_Attribute (Node.IML_Node, Attribute);
                  end loop;

               end if;
               Load_Nodes.Node_Queues.MoveNext (Iter);
            end loop;
         end ProcessQueue;

         Root_Node : Storables.Storable;
         Created   : Boolean;
         Node      : Load_Nodes.Node_Access;

      begin
         Queue := Load_Nodes.Node_Queues.Create;

         Root_Node := Storables.Storable
           (IML_Graphs.Get_Raw_Graph (IML_Graph));

         Iml_Node_Mapper.Get (Root_Node, Node, Created);

         Load_Nodes.Node_Queues.Attach (Queue, Node);

         ProcessQueue;
      end ConvertIMLGraphToTempStructure;

      -------------------------------------------------------------------------
      --  Converts generated temporary structure to the structure
      --  which is defined by Node_Record etc.
      --
      --  Parameters:
      --    Queue: Queue containing all nodes in temporary structure
      procedure ConvertTempStructureToUsedStructure
        (Queue : in Load_Nodes.Node_Queue) is

         --  Precondition:
         --    Size of TargtArray == Length(SourceList)
         procedure ConvertEdges
           (SourceList  : in     Load_Nodes.Edge_Lists.List;
            TargetArray :    out Edge_Id_Array) is

            EdgeIter : Load_Nodes.Edge_Lists.ListIter;
            CurEdge  : Load_Nodes.Edge_Access;

         begin
            EdgeIter := Load_Nodes.Edge_Lists.MakeListIter (SourceList);

            for I in TargetArray'Range loop
               Load_Nodes.Edge_Lists.Next (EdgeIter, CurEdge);

               TargetArray (I) := new Edge_Record;
               TargetArray (I).Source_Node := CurEdge.Source.Internal_Node;
               TargetArray (I).Target_Node := CurEdge.Target.Internal_Node;
               TargetArray (I).Attribute   := CurEdge.Attribute;
               TargetArray (I).Attribute_Element_Number :=
                 CurEdge.Attribute_Element_Number;
            end loop;
         end ConvertEdges;

         NodeIter : Load_Nodes.Node_Queues.ListIter;
         CurNode  : Load_Nodes.Node_Access; --  of the temporary structure

         NewNode  : Node_Id; --  of the internal structure

      begin
         IML_Node_ID_Mapping := IML_Node_ID_Hashed_Mappings.Create;

         --  Convert nodes
         NodeIter := Load_Nodes.Node_Queues.MakeListIter (Queue);
         while Load_Nodes.Node_Queues.More (NodeIter) loop
            Load_Nodes.Node_Queues.Next (NodeIter, CurNode);

            NewNode := new Node_Record
              (Number_Of_Incoming_Edges =>
                 Load_Nodes.Edge_Lists.Length (CurNode.Edges_In),
               Number_Of_Outgoing_Edges =>
                 Load_Nodes.Edge_Lists.Length (CurNode.Edges_Out)
               );

            NewNode.IML_Node := CurNode.IML_Node;

            IML_Node_ID_Hashed_Mappings.Bind
              (IML_Node_ID_Mapping,
               Storables.Get_Node_ID (NewNode.IML_Node),
               NewNode);

            --  set variable to enable edge-conversion
            CurNode.Internal_Node := NewNode;
         end loop;

         --  Convert edges
         NodeIter := Load_Nodes.Node_Queues.MakeListIter (Queue);
         while Load_Nodes.Node_Queues.More (NodeIter) loop
            Load_Nodes.Node_Queues.Next (NodeIter, CurNode);

            ConvertEdges( CurNode.Edges_In,  NewNode.Incoming_Edges);
            ConvertEdges( CurNode.Edges_Out, NewNode.Outgoing_Edges);
         end loop;

      end ConvertTempStructureToUsedStructure;

      -------------------------------------------------------------------------
      --  Destroys the temporary structure,
      --  frees all memory
      --  Queue is deallocated, too
      procedure DestroyTemporaryStructure
        (Queue : in out Load_Nodes.Node_Queue) is

         procedure Dispose
            (Node  : in out Load_Nodes.Node_Access) is
         begin
            Load_Nodes.Destroy_Node (Node);
         end Dispose;

         procedure DestroyDeep_Nodes is new
           Load_Nodes.Node_Queues.DestroyDeep (Dispose => Dispose);

      begin
         DestroyDeep_Nodes (Queue);
      end DestroyTemporaryStructure;

   begin
      --  Load Graph into memory
      begin
         IML_Graph := IML.IO.Load (Path_To_IML_File);
      exception
         when Storables.Load_Failure =>
            raise Load_Error;
      end;

      Iml_Node_Mapper.Create;

      declare
         Queue : Load_Nodes.Node_Queue;
      begin
         ConvertIMLGraphToTempStructure (Queue);
         ConvertTempStructureToUsedStructure (Queue);
         DestroyTemporaryStructure (Queue);
      end;

      Iml_Node_Mapper.Destroy;

   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy is

      procedure DestroyAllNodes is

         procedure FreeEdgeId is new Ada.Unchecked_Deallocation
           (Edge_Record,
            Edge_Id);

         procedure FreeNodeId is new Ada.Unchecked_Deallocation
           (Node_Record,
            Node_Id);

         Iter    : IML_Node_ID_Hashed_Mappings.Values_Iter;
         CurNode : Node_Id;
      begin
         --  There is no Hashed_Mappings.DestroyDeep, therefore we have to do
         --  this by hand

         Iter := IML_Node_ID_Hashed_Mappings.Make_Values_Iter
           (IML_Node_ID_Mapping);

         while IML_Node_ID_Hashed_Mappings.More (Iter) loop
            IML_Node_ID_Hashed_Mappings.Next (Iter, CurNode);

            --  remove all the edges out of the memory
            for I in CurNode.Outgoing_Edges'Range loop
               FreeEdgeId (CurNode.Outgoing_Edges (I));
            end loop;

            FreeNodeId (CurNode);

         end loop;

         IML_Node_ID_Hashed_Mappings.Destroy (IML_Node_ID_Mapping);
      end DestroyAllNodes;

   begin
      DestroyAllNodes;

      IML_Node_ID_Hashed_Mappings.Destroy (IML_Node_ID_Mapping);

      --  Unload IML_Graph - not supported by IML
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
      IML_Node_Id : IML_Node_IDs.Node_Id;
   begin
      IML_Node_Id := Storables.Get_Node_Id (Node.Iml_Node);

      return IML_Node_IDs.Image (IML_Node_Id);
   end Node_Id_Image;

   ---------------------------------------------------------------------------
   --  Implemented with 'Image, since IML_Node_IDs does not offer "Value"
   --
   --  Precondition:
   --    IML_Node_IDs.Node_IDs are stored internally as strings
   function Node_Id_Value
     (Node : in String)
     return Node_Id
   is
      P           : Positive;
      IML_Node_ID : IML_Node_IDs.Node_ID;
      Value       : Node_Id;
   begin
      begin
         P := Positive'Value (Node);
      exception
         --  following could be done better using different exceptions for
         --  different errors, but I don't think, that such a differenciation
         --  makes sence
         when others => raise Node_Does_Not_Exist;
      end;

      IML_Node_ID := IML_Node_IDs.Make_Node_ID (P);

      Value :=
        IML_Node_ID_Hashed_Mappings.Fetch
        (IML_Node_ID_Mapping,
         IML_Node_ID);

      return Value;
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
                 := IML_Reflection.SLoc_Field (Attribute.all);
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
      -- FIX: return the real root
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

