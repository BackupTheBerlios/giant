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
--  $RCSfile: giant-graph_lib.adb,v $, $Revision: 1.42 $
--  $Author: koppor $
--  $Date: 2003/07/04 15:11:55 $

--  TBD:
--    s/Program_Error/somethingofmyown

--  from ADA
with Ada.Unchecked_Deallocation;

--  from Bauhaus
with Tagged_Ptr_Hash;
with Tagged_Constant_Ptr_Ops;
with Untagged_Ptr_Ops;

with SLocs;
with Storables;
with IML_Classes;
with IML_Graphs;
with IML.IO;
with IML_Reflection;
with IML_Roots;
with Lists;

--  from Giant
with Giant.Ptr_Normal_Hashs;
with Giant.Logger;

pragma Elaborate_All (Giant.Ptr_Normal_Hashs);
pragma Elaborate_All (Tagged_Ptr_Hash);

package body Giant.Graph_Lib is

   package My_Logger is new Logger("giant.graph_lib");

   Invalid_Node_Attribute_Id : constant Node_Attribute_Id := null;

   --  reference to loaded IML-Graph
   --  Created in "Create"
   --  Destroyed in "Destroy"
   IML_Graph : IML_Graphs.IML_Graph;

   package Edge_Id_Array_Routines is new Edge_Id_Sets.Arrays
     (Item_Array => Edge_Id_Array);

   --------------------------------------
   --  Hashing for Node_Attribute_Ids  --
   --------------------------------------

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

   ---------------------
   --  Miscellaneous  --
   ---------------------

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Node_Id;
      Right : Node_Id)
     return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Node_Record,
         T_Ptr => Node_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is

      package Compare is new Tagged_Constant_Ptr_Ops
        (T     => IML_Reflection.Abstract_Class'Class,
         T_Ptr => Node_Class_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
     (Left  : Edge_Id;
      Right : Edge_Id)
      return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Edge_Record,
         T_Ptr => Edge_Id);

   begin
      return Compare.Less (Left, Right);
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

      package Compare is new Untagged_Ptr_Ops
        (T     => Edge_Class,
         T_Ptr => Edge_Class_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "<"
      (Left  : Node_Attribute_Id;
       Right : Node_Attribute_Id)
      return Boolean
   is

      package Compare is new Tagged_Constant_Ptr_Ops
        (T     => IML_Reflection.Field'Class,
         T_Ptr => Node_Attribute_Id);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   function "="
     (Left  : Node_Class_Id;
      Right : Node_Class_Id)
      return Boolean
   is
   begin
      return IML_Reflection."=" (Left, Right);
   end "=";

   ---------------------------------------------------------------------------
   function Convert_Node_Attribute_Id_To_Name
     (Node_Attribute : in Node_Attribute_Id)
      return String
   is
   begin
      return Node_Attribute.Name;
   end Convert_Node_Attribute_Id_To_Name;

   ---------------------------------------------------------------------------
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id
   is
      Node_Class     : Node_Class_Id;
   begin
      Node_Class := Convert_Node_Class_Name_To_Id (Node_Class_Name);

      return Convert_Node_Attribute_Name_To_Id
        (Node_Class, Node_Attribute_Name);
   end Convert_Node_Attribute_Name_To_Id;

   ---------------------------------------------------------------------------
   --  Performance can be increased if no linear search, but a hasing is used
   --    (In create() the hasing should be instantiated)
   --
   --  Case-senstive search
   function Convert_Node_Attribute_Name_To_Id
     (Node_Class          : in Node_Class_Id;
      Node_Attribute_Name : in String)
     return Node_Attribute_Id
   is
      I         : Integer;
   begin
      I := Node_Class.Fields'First;
      while
        (I <= Node_Class.Fields'Last) and then
        (Node_Class.Fields (I).Name /= Node_Attribute_Name) loop
         I := I + 1;
      end loop;

      if I <= Node_Class.Fields'Last then
         return Node_Class.Fields(I);
      else
         raise Node_Attribute_Does_Not_Exist;
      end if;
   end Convert_Node_Attribute_Name_To_Id;

   ---------------------------------------------------------------------------
   function Convert_Node_Class_Name_To_Id
     (Node_Class_Name : in String)
      return Node_Class_Id
   is
      All_Classes : IML_Reflection.Classes;
      I           : Integer;
   begin
      --  straightforward-implementation, since this function is only
      --  needed during the initialization

      All_Classes := IML_Classes.Get_All_Classes;

      I := All_Classes'First;
      while
        (I <= All_Classes'Last) and then
        (All_Classes (I).Name /= Node_Class_Name) loop
         I := I+1;
      end loop;

      if I <= All_Classes'Last then
         return All_Classes (I);
      else
         raise Node_Class_Does_Not_Exist;
      end if;
   end Convert_Node_Class_Name_To_Id;

   ---------------------------------------------------------------------------
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

   -----------------------------
   --  Basical init-routines  --
   -----------------------------

   ---------------------------------------------------------------------------
   procedure Initialize
   is
      All_Classes : IML_Reflection.Classes;
      Cur_Class   : Node_Class_Id;
      Hash_Data   : Node_Class_Id_Hash_Data_Access;
   begin
      Node_Class_Id_Mapping := Node_Class_Id_Hashed_Mappings.Create;

      All_Classes := IML_Classes.Get_All_Classes;

      for I in All_Classes'Range loop
         Cur_Class := All_Classes (I);

         Hash_Data := new Node_Class_Id_Hash_Data;
         Hash_Data.Node_Attribute_Id_Mapping :=
           Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Create;

         Node_Class_Id_Hashed_Mappings.Bind
              (Node_Class_Id_Mapping,
               Cur_Class,
               Hash_Data);

         for J in Cur_Class.Fields'Range loop
            declare
               Cur_Field : IML_Reflection.Field_Id := Cur_Class.Fields (J);
            begin
               if (Cur_Field.all in IML_Reflection.Edge_Field) or
                 (Cur_Field.all in IML_Reflection.List_Field) or
                 (Cur_Field.all in IML_Reflection.Set_Field) then
                  declare
                     New_Edge_Class : Edge_Class_Id;
                  begin
                     New_Edge_Class := new Edge_Class;
                     New_Edge_Class.Source_Node_Class := Cur_Class;
                     New_Edge_Class.Source_Node_Attribute := Cur_Field;

                     Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Bind
                       (Hash_Data.Node_Attribute_Id_Mapping,
                        Cur_Field,
                        New_Edge_Class);
                  end;
               end if;
            end;
         end loop;

      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Destroy
   is
   begin
      --  Destroy_Node_Class_Id_Mapping
      --  TBD: deep-destroy!
      null;
   end Destroy;

   -----------------------------------------------------------
   --  Create the internal representation of the IML-Graph  --
   -----------------------------------------------------------
   procedure Load (Path_To_IML_File : in String) is

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

            --  Used at conversion from temporary structure to
            --  graph_lib-internal structure
            Internal_Edge : Edge_Id;
         end record;

         type Edge_Access is access Edge_Record;

         package Edge_Lists is new Lists
           (ItemType => Edge_Access);

         type Node_Record is record
            Edges_In      : Edge_Lists.List;
            Edges_Out     : Edge_Lists.List;
            IML_Node      : Storables.Storable;

            --  Used at conversion from temporary structure to
            --  graph_lib-internal structure
            Internal_Node : Node_Id;
         end record;

         --  Creates an edge in the package-internal structure
         procedure Create_Edge
           (Edge_Source              : in Node_Access;
            Edge_Target              : in Node_Access;
            Attribute                : in Node_Attribute_Id;
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

         package Node_Queues is new Lists (ItemType => Node_Access);
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
            --  initialize edge
            Edge.Source      := Edge_Source;
            Edge.Target      := Edge_Target;
            Edge.Attribute   := Attribute;
            Edge.Attribute_Element_Number := Attribute_Element_Number;

            --  make it known to connected nodes
            Edge_Lists.Attach (Edge, Edge_Target.Edges_In);
            Edge_Lists.Attach (Edge, Edge_Source.Edges_Out);
         end Create_Edge;

         function Create_Node
           (IMLNode : Storables.Storable)
           return Node_Access is
            New_Node : Node_Access := new Node_Record;
         begin
            New_Node.IML_Node  := IMLNode;
            New_Node.Edges_In  := Edge_Lists.Create;
            New_Node.Edges_Out := Edge_Lists.Create;
            return New_Node;
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

         function Storable_Hash is new Tagged_Ptr_Hash
           (T     => Storables.Storable_Class,
            T_Ptr => Storables.Storable);

         package Mapping_Iml_LoadNodes is new Hashed_Mappings
           (Key_Type   => Storables.Storable,
            Hash       => Storable_Hash,
            Value_Type => Load_Nodes.Node_Access);

         Mapping : Mapping_Iml_LoadNodes.Mapping;

         procedure Create is
         begin
            Mapping := Mapping_Iml_LoadNodes.Create (19);
            -- FIX: breaks when (19) is passed
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
      procedure Convert_IML_Graph_To_Temp_Structure
        (Queue : out Load_Nodes.Node_Queue)
      is

         procedure Process_Queue is

            --  used for getting a much better performance
            --    at Node_Queues.Attach
            Queue_Tail    : Load_Nodes.Node_Queues.List;

            --  Does all necessary things, if there's an edge from
            --    a Node to an IML_Node
            procedure Process_Edge
              (Source_Node : in Load_Nodes.Node_Access;
               Target      : in Storables.Storable;
               Attribute   : in Node_Attribute_Id;
               Attribute_Element_Number : in Natural) is

               Created     : Boolean;
               Target_Node : Load_Nodes.Node_Access;

               New_Queue_Tail : Load_Nodes.Node_Queues.List;

            begin
               --  My_Logger.Debug ("Begin: Process_Edge");

               IML_Node_Mapper.Get (Target, Target_Node,  Created);

               --  My_Logger.Debug ("Fetched Target_Node");

               if Created then
                  New_Queue_Tail := Load_Nodes.Node_Queues.MakeList
                    (Target_Node);
                  Load_Nodes.Node_Queues.Attach (Queue_Tail, New_Queue_Tail);
                  Queue_Tail := New_Queue_Tail;
                  --  My_Logger.Debug ("Attached Target_Node");
               end if;

               Load_Nodes.Create_Edge
                 (Source_Node,
                  Target_Node,
                  Attribute,
                  Attribute_Element_Number);

               --  My_Logger.Debug ("Edge created");

               --  My_Logger.Debug ("End: Process_Edge");
            end Process_Edge;

            Node  : Load_Nodes.Node_Access;

            procedure Process_Attribute
              (IML_Node  : in Storables.Storable;
               Attribute : in IML_Reflection.Field_ID) is

               Target : Storables.Storable;
            begin
               --  My_Logger.Debug ("Begin: Process_Attribute");

               if Attribute.all in IML_Reflection.Edge_Field'Class then
                  declare
                     IML_Edge : IML_Reflection.Edge_Field
                       := IML_Reflection.Edge_Field (Attribute.all);
                  begin
                     Target   := IML_Edge.Get_Target
                       (IML_Roots.IML_Root (IML_Node));
                     if Storables."/=" (Target, null) then
                        Process_Edge (Node, Target, Attribute, 0);
                        --  else
                        --    My_Logger.Info
                        --      ("Edge_Field with null target ignored");
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

                     IML_Reflection.Destroy (Iter);
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
                        IML_Reflection.Next (Iter, Target);
                        I := I+1;

                        Process_Edge (Node, Target, Attribute, I);
                     end loop;

                     IML_Reflection.Destroy (Iter);
                  end;
               elsif
                 (Attribute.all in
                  IML_Reflection.Identifier_Field'Class) or
                 (Attribute.all in
                  IML_Reflection.Builtin_Field'Class) then
                  null;
               else
                  My_Logger.Error (Attribute.Name);
                  My_Logger.Error ("Unknown IML_Reflection.Field");
               end if;

               --  My_Logger.Debug ("End: Process_Attribute");
            end Process_Attribute;

            Iter      : Load_Nodes.Node_Queues.ListIter;

            Class     : IML_Reflection.Class_ID;
            Attribute : IML_Reflection.Field_ID;

         begin
            Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);

            Queue_Tail := Queue;

            while Load_Nodes.Node_Queues.More (Iter) loop
               Node := Load_Nodes.Node_Queues.CellValue (Iter);

               --  My_Logger.Debug ("Processing: " &
               --                     IML_Node_Ids.Image
               --        (Storables.Get_Node_Id (Node.IML_Node)));

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
               Load_Nodes.Node_Queues.Forward (Iter);
            end loop;
         end Process_Queue;

         Root_Node : Storables.Storable;
         Created   : Boolean;
         Node      : Load_Nodes.Node_Access;

      begin
         My_Logger.Debug ("Begin: Convert_IML_Graph_To_Temp_Structure");

         Queue := Load_Nodes.Node_Queues.Create;

         Root_Node := Storables.Storable
           (IML_Graphs.Get_Raw_Graph (IML_Graph));

         Iml_Node_Mapper.Get (Root_Node, Node, Created);

         Load_Nodes.Node_Queues.Attach (Queue, Node);

         Process_Queue;
      end Convert_IML_Graph_To_Temp_Structure;

      -------------------------------------------------------------------------
      --  Converts generated temporary structure to the structure
      --  which is defined by Node_Record etc.
      --
      --  Parameters:
      --    Queue: Queue containing all nodes in temporary structure
      procedure Convert_Temp_Structure_To_Used_Structure
        (Queue : in Load_Nodes.Node_Queue) is

         ---------------------------------------------------------------------
         --  Converts temporary nodes to internal nodes
         --
         --  Parameters:
         --    IML_Node_ID_Mapping': Contains mapping from IML to
         --                          internal nodes created in this procedure
         procedure Convert_Nodes
           (IML_Node_ID_Mapping : out IML_Node_Id_Hashed_Mappings.Mapping)
         is

            Node_Iter : Load_Nodes.Node_Queues.ListIter;
            Cur_Node  : Load_Nodes.Node_Access; --  of the temporary structure
            New_Node  : Node_Id; --  of the internal structure

         begin
            IML_Node_ID_Mapping := IML_Node_ID_Hashed_Mappings.Create;

            Node_Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);
            while Load_Nodes.Node_Queues.More (Node_Iter) loop
               Load_Nodes.Node_Queues.Next (Node_Iter, Cur_Node);

               New_Node := new Node_Record
                 (Number_Of_Incoming_Edges =>
                    Load_Nodes.Edge_Lists.Length (Cur_Node.Edges_In),
                  Number_Of_Outgoing_Edges =>
                    Load_Nodes.Edge_Lists.Length (Cur_Node.Edges_Out)
                  );

               New_Node.IML_Node := Cur_Node.IML_Node;

               IML_Node_ID_Hashed_Mappings.Bind
                 (IML_Node_ID_Mapping,
                  Storables.Get_Node_ID (New_Node.IML_Node),
                  New_Node);

               --  set variable to enable edge-conversion
               Cur_Node.Internal_Node := New_Node;
            end loop;
         end Convert_Nodes;

         ----------------------------------------------------------------------
         --  In this step, all Edge_Records are created and linked to the
         --    temporary edges
         --
         --  Parameters:
         --    All_Edges_Set': Contains all created edges
         procedure Convert_Outgoing_Edges
           (All_Edges_Set : out Edge_Id_Sets.Set)
         is

            --  up to now, we haven't used any index
            --  later, the index ist /first/ incremented and then set
            --  therefore it is started by one index before the first index
            --  of the All_Edges-Array.
            Last_Internal_Id : Integer := All_Edges_First_Index - 1;

            -------------------------------------------------------------------
            --  Modifies "All_Edges_Set"
            --    adds created edges to "All_Edges_Set"
            --  Precondition:
            --    Size of TargtArray == Length(Source_List)
            procedure Convert_Outgoing_Edges_Of_A_Node
              (Source_List  : in     Load_Nodes.Edge_Lists.List;
               Target_Array :    out Edge_Id_Array) is

               Edge_Iter : Load_Nodes.Edge_Lists.ListIter;
               Cur_Edge  : Load_Nodes.Edge_Access;

            begin
               pragma Assert (Target_Array'Length =
                                Load_Nodes.Edge_Lists.Length (Source_List));

               Edge_Iter := Load_Nodes.Edge_Lists.MakeListIter (Source_List);

               for I in Target_Array'Range loop
                  Load_Nodes.Edge_Lists.Next (Edge_Iter, Cur_Edge);

                  --  Every new edge gets a higher id
                  Last_Internal_Id := Last_Internal_Id + 1;

                  Target_Array (I) :=
                    new Edge_Record;
                  Target_Array (I).Internal_Id :=
                    Last_Internal_Id;
                  Target_Array (I).Source_Node :=
                    Cur_Edge.Source.Internal_Node;
                  Target_Array (I).Target_Node :=
                    Cur_Edge.Target.Internal_Node;
                  Target_Array (I).Attribute   :=
                    Cur_Edge.Attribute;
                  Target_Array (I).Attribute_Element_Number :=
                    Cur_Edge.Attribute_Element_Number;

                  --  store in temporary edge, the belonging package-internal
                  --    one ("backreference")
                  Cur_Edge.Internal_Edge := Target_Array (I);

                  --  add it to procedure-internal set holding all edges
                  --  of the graph
                  Edge_Id_Sets.Insert (All_Edges_Set, Target_Array (I));
               end loop;
            end Convert_Outgoing_Edges_Of_A_Node;

            Node_Iter : Load_Nodes.Node_Queues.ListIter;
            Cur_Node  : Load_Nodes.Node_Access; --  of the temporary structure
            New_Node  : Node_Id; --  of the internal structure

         begin
            All_Edges_Set := Edge_Id_Sets.Empty_Set;

            Node_Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);
            while Load_Nodes.Node_Queues.More (Node_Iter) loop
               Load_Nodes.Node_Queues.Next (Node_Iter, Cur_Node);

               New_Node := Cur_Node.Internal_Node;

               Convert_Outgoing_Edges_Of_A_Node
                 (Cur_Node.Edges_In,
                  New_Node.Incoming_Edges);
            end loop;
         end Convert_Outgoing_Edges;

         ---------------------------------------------------------------------
         --  The structure of this routine is similar to the one of
         --    Convert_Outgoing_Edges
         procedure Convert_Incoming_Edges
         is

            procedure Convert_Incoming_Edges_Of_A_Node
              (Source_List  : in     Load_Nodes.Edge_Lists.List;
               Target_Array :    out Edge_Id_Array) is

               Edge_Iter : Load_Nodes.Edge_Lists.ListIter;
               Cur_Edge  : Load_Nodes.Edge_Access;

            begin
               pragma Assert (Target_Array'Length =
                                Load_Nodes.Edge_Lists.Length (Source_List));

               Edge_Iter := Load_Nodes.Edge_Lists.MakeListIter (Source_List);

               for I in Target_Array'Range loop
                  Load_Nodes.Edge_Lists.Next (Edge_Iter, Cur_Edge);

                  --  In Convert_Outoing_Edges_Of_A_Node
                  --    Internal_Edge was set to the belonging
                  --    package-internal edge, so only a copy of this
                  --    reference is necessary
                  --  This assumption is valid, since each incoming edge
                  --    is also an outgoing edge of another node and vice versa
                  Target_Array (I) := Cur_Edge.Internal_Edge;
               end loop;
            end Convert_Incoming_Edges_Of_A_Node;

            Node_Iter : Load_Nodes.Node_Queues.ListIter;
            Cur_Node  : Load_Nodes.Node_Access; --  of the temporary structure
            New_Node  : Node_Id; --  of the internal structure
         begin
            Node_Iter := Load_Nodes.Node_Queues.MakeListIter (Queue);
            while Load_Nodes.Node_Queues.More (Node_Iter) loop
               Load_Nodes.Node_Queues.Next (Node_Iter, Cur_Node);

               New_Node := Cur_Node.Internal_Node;

               Convert_Incoming_Edges_Of_A_Node
                 (Cur_Node.Edges_In,
                  New_Node.Incoming_Edges);
            end loop;
         end Convert_Incoming_Edges;

         ----------------------------------------------------------------------
         --  Convert set containing all edges to "public" array of all edges
         --
         --  The conversion cannot be done by Edge_Id_Array_Routines.To_Array
         --    since this stores the array on the stack, which could be
         --    too small (cp Revision 1.36)
         --
         --  Parameters:
         --     First_Index - specifies the index to be used as first
         --                   in the resulting array
         function Convert_Edge_Set_To_Edge_Array
           (Edges_Set   : in Edge_Id_Sets.Set;
            First_Index : in Integer)
           return Edge_Id_Array_Access
         is

            Edge_Array         : Edge_Id_Array_Access;
            Current_Edge_Index : Natural;

            procedure Execute (Edge : Edge_Id)
            is
            begin
               Current_Edge_Index := Current_Edge_Index + 1;
               Edge_Array (Current_Edge_Index) := Edge;
            end Execute;

            procedure Apply is new Edge_Id_Sets.Apply
              (Execute => Execute);

          begin
             Edge_Array := new Edge_Id_Array
               (First_Index..Edge_Id_Sets.Size (Edges_Set) - First_Index + 1);
             Current_Edge_Index := Edge_Array'First - 1;
             Apply (Edges_Set);
             return Edge_Array;
          end Convert_Edge_Set_To_Edge_Array;

      begin
         My_Logger.Debug ("Begin: Convert_Temp_Structure_To_Used_Structure");

         Convert_Nodes (IML_Node_ID_Mapping);

         --  all nodes are existing now
         --  create edges
         declare
            All_Edges_Set : Edge_Id_Sets.Set;
         begin
            Convert_Outgoing_Edges (All_Edges_Set);

            --  Convert set containing all edges to "public" array of all edges
            All_Edges := Convert_Edge_Set_To_Edge_Array
              (All_Edges_Set, All_Edges_First_Index);

            Edge_Id_Sets.Destroy (All_Edges_Set);
         end;

         Convert_Incoming_Edges;
      end Convert_Temp_Structure_To_Used_Structure;

      -------------------------------------------------------------------------
      --  Destroys the temporary structure,
      --  frees all memory
      --  Queue is deallocated, too
      procedure Destroy_Temp_Structure
        (Queue : in out Load_Nodes.Node_Queue) is

         procedure Dispose
            (Node  : in out Load_Nodes.Node_Access) is
         begin
            Load_Nodes.Destroy_Node (Node);
         end Dispose;

         procedure DestroyDeep_Nodes is new
           Load_Nodes.Node_Queues.DestroyDeep (Dispose => Dispose);

      begin
         My_Logger.Debug ("Begin: Destroy_Temp_Structure");
         DestroyDeep_Nodes (Queue);
      end Destroy_Temp_Structure;

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
         Convert_IML_Graph_To_Temp_Structure (Queue);
         Convert_Temp_Structure_To_Used_Structure (Queue);
         Destroy_TEmp_Structure (Queue);
      end;

      Iml_Node_Mapper.Destroy;
   end Load;

   ---------------------------------------------------------------------------
   procedure Unload is

      procedure Destroy_Internal_Graph is

         procedure Free_Edge_Id is new Ada.Unchecked_Deallocation
           (Edge_Record,
            Edge_Id);

         procedure Free_Node_Id is new Ada.Unchecked_Deallocation
           (Node_Record,
            Node_Id);

         Iter    : IML_Node_ID_Hashed_Mappings.Values_Iter;
         Cur_Node : Node_Id;
      begin
         --  There is no Hashed_Mappings.DestroyDeep, therefore we have to do
         --  this by hand

         Iter := IML_Node_ID_Hashed_Mappings.Make_Values_Iter
           (IML_Node_ID_Mapping);

         while IML_Node_ID_Hashed_Mappings.More (Iter) loop
            IML_Node_ID_Hashed_Mappings.Next (Iter, Cur_Node);

            --  remove all the edges out of the memory
            for I in Cur_Node.Outgoing_Edges'Range loop
               Free_Edge_Id (Cur_Node.Outgoing_Edges (I));
            end loop;

            Free_Node_Id (Cur_Node);

         end loop;

         --  The mapping was created at
         --    Convert_Temp_Structure_To_Used_Structure
         IML_Node_ID_Hashed_Mappings.Destroy (IML_Node_ID_Mapping);
      end Destroy_Internal_Graph;

   begin
      Destroy_Internal_Graph;

      --  Unload IML_Graph from memory
      --  FIXME:  IML_Graphs.Destroy (IML_Graph);
      --          crashes if used
   end Unload;

   ---------------------------------------------------------------------------
   function Does_Edge_Class_Exist
     (Node_Class     : in Node_Class_Id;
      Node_Attribute : in Node_Attribute_Id)
      return Boolean
   is
      Hash_Data : Node_Class_Id_Hash_Data_Access;
   begin
      if not Node_Class_Id_Hashed_Mappings.Is_Bound
        (Node_Class_Id_Mapping,
         Node_Class) then
         return False;
      else
         Hash_Data := Node_Class_Id_Hashed_Mappings.Fetch
           (Node_Class_Id_Mapping,
            Node_Class);

         return Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.Is_Bound
           (Hash_Data.Node_Attribute_Id_Mapping,
            Node_Attribute);
      end if;
   end Does_Edge_Class_Exist;

   ---------------------------------------------------------------------------
   function Does_Node_Attribute_Exist
     (Node_Class_Name     : in String;
      Node_Attribute_Name : in String)
      return Boolean
   is
   begin
      declare
         Attribute : Node_Attribute_Id;
      begin
         Attribute := Convert_Node_Attribute_Name_To_Id
           (Node_Class_Name,
            Node_Attribute_Name);

         --  No exception: Attribute exists
         return True;
      exception
         when Node_Attribute_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Attribute_Exist;

   ---------------------------------------------------------------------------
   function Does_Node_Class_Exist
     (Node_Class_Name : in String)
      return Boolean
   is
   begin
      declare
         Node_Class : Node_Class_Id;
      begin
         Node_Class := Convert_Node_Class_Name_To_Id (Node_Class_Name);

         --  no exception risen: class exists
         return True;
      exception
         when Node_Class_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Class_Exist;

   ---------------------------------------------------------------------------
   --  Loops over the hashtables storing the Edgeclasses and returns all
   --    of them
   function Get_All_Edge_Class_Ids return Edge_Class_Id_Set is
      Set      : Edge_Class_Id_Set;

      IterData : Node_Class_Id_Hashed_Mappings.Values_Iter;
      CurData  : Node_Class_Id_Hash_Data_Access;

      --  used for the inner loop
      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Values_Iter;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set      := Edge_Class_Id_Sets.Empty_Set;
      IterData := Node_Class_Id_Hashed_Mappings.Make_Values_Iter
        (Node_Class_Id_Mapping);

      while Node_Class_Id_Hashed_Mappings.More (IterData) loop
         Node_Class_Id_Hashed_Mappings.Next (IterData, CurData);

         IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Make_Values_Iter
           (CurData.Node_Attribute_Id_Mapping);

         while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
           .More (IterAttrib) loop
            Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
              Next (IterAttrib, Cur_Edge_Class);

            Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
         end loop;
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids;

   ---------------------------------------------------------------------------
   function Get_Node_Class_Tag
     (Node_Class : in Node_Class_Id)
     return String
   is
   begin
      return Node_Class.Name;
   end Get_Node_Class_Tag;

   ---------------------------------------------------------------------------
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

      --  Fixed by Martin: ensures that "Node_Does_Not_Exist" will be raised
      --  if the node id is not part of the hash map.
      if not IML_Node_ID_Hashed_Mappings.Is_Bound
        (IML_Node_ID_Mapping,
         IML_Node_ID) then

         raise Node_Does_Not_Exist;
      end if;

      Value :=
        IML_Node_ID_Hashed_Mappings.Fetch
        (IML_Node_ID_Mapping,
         IML_Node_ID);

      return Value;
   end Node_Id_Value;

   ---------------------------------------------------------------------------
   function Does_Node_Id_Exist
     (Node : in String)
     return Boolean
   is
   begin
      declare
         ID : Node_Id;
      begin
         ID := Node_Id_Value (Node);

         return True;
      exception
         when Node_Does_Not_Exist =>
            return False;
      end;
   end Does_Node_Id_Exist;

   ---------------------------------------------------------------------------
   function Get_All_Edges
     return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (All_Edges.all);
   end Get_All_Edges;

   ---------------------------------------------------------------------------
   function Get_All_Edge_Class_Ids_For_Node_Attribute
     (Node_Attribute_Name : in String)
      return Edge_Class_Id_Set
   is
      Set      : Edge_Class_Id_Set;

      IterData : Node_Class_Id_Hashed_Mappings.Values_Iter;
      CurData  : Node_Class_Id_Hash_Data_Access;

      --  used for the inner loop
      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Bindings_Iter;
      CurAttribute   : Node_Attribute_Id;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set      := Edge_Class_Id_Sets.Empty_Set;
      IterData := Node_Class_Id_Hashed_Mappings.Make_Values_Iter
        (Node_Class_Id_Mapping);

      while Node_Class_Id_Hashed_Mappings.More (IterData) loop
         Node_Class_Id_Hashed_Mappings.Next (IterData, CurData);

         IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Make_Bindings_Iter
           (CurData.Node_Attribute_Id_Mapping);

         while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
           .More (IterAttrib) loop
            Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
              Next (IterAttrib, CurAttribute, Cur_Edge_Class);

            if CurAttribute.Name = Node_Attribute_Name then
               Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
            end if;
         end loop;
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids_For_Node_Attribute;

   ---------------------------------------------------------------------------
   function Get_All_Edge_Class_Ids_For_Node_Class
     (Node_Class : in Node_Class_Id)
      return Edge_Class_Id_Set
   is
      Set       : Edge_Class_Id_Set;
      ClassData : Node_Class_Id_Hash_Data_Access;

      IterAttrib : Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Values_Iter;
      Cur_Edge_Class  : Edge_Class_Id;

   begin
      Set := Edge_Class_Id_Sets.Empty_Set;

      ClassData := Node_Class_Id_Hashed_Mappings.Fetch
        (Node_Class_Id_Mapping, Node_Class);

      IterAttrib := Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
        Make_Values_Iter (ClassData.Node_Attribute_Id_Mapping);

      while Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings
        .More (IterAttrib) loop
         Node_Attribute_Id_To_Edge_Class_Id_Hashed_Mappings.
           Next (IterAttrib, Cur_Edge_Class);

         Edge_Class_Id_Sets.Insert (Set, Cur_Edge_Class);
      end loop;

      return Set;
   end Get_All_Edge_Class_Ids_For_Node_Class;

   ---------------------------------------------------------------------------
   function Get_All_Node_Class_Ids return Node_Class_Id_Set is
      Set      : Node_Class_Id_Set;

      Iter     : Node_Class_Id_Hashed_Mappings.Keys_Iter;
      CurClass : Node_Class_Id;

   begin
      Set  := Node_Class_Id_Sets.Empty_Set;
      Iter := Node_Class_Id_Hashed_Mappings.Make_Keys_Iter
        (Node_Class_Id_Mapping);

      while Node_Class_Id_Hashed_Mappings.More (Iter) loop
         Node_Class_Id_Hashed_Mappings.Next (Iter, CurClass);
         Node_Class_Id_Sets.Insert (Set, CurClass);
      end loop;

      return Set;
   end Get_All_Node_Class_Ids;

   ----------------------------------------------------------------------------
   --  Analogue to DestroyAllNodes
   function Get_All_Nodes
      return Node_Id_Set
   is
      Set      : Node_Id_Set;
      Iter     : IML_Node_ID_Hashed_Mappings.Values_Iter;
      Cur_Node : Node_Id;

   begin
      Set := Node_Id_Sets.Empty_Set;

      Iter := IML_Node_ID_Hashed_Mappings.Make_Values_Iter
        (IML_Node_ID_Mapping);

      while IML_Node_ID_Hashed_Mappings.More (Iter) loop
         IML_Node_ID_Hashed_Mappings.Next (Iter, Cur_Node);

         Node_Id_Sets.Insert (Set, Cur_Node);
      end loop;

      return Set;
   end Get_All_Nodes;

   ---------------------------------------------------------------------------
   function Get_Edge_Count
     return Natural
   is
   begin
      return All_Edges'Length;
   end Get_Edge_Count;

   ---------------------------------------------------------------------------
   function Get_Node_Count
     return Natural
   is
   begin
      return IML_Node_ID_Hashed_Mappings.Size (IML_Node_ID_Mapping);
   end Get_Node_Count;

   ---------------------------------------------------------------------------
   function Get_Edge_Class_Id
     (Edge : in Edge_Id)
      return Edge_Class_Id
   is
   begin
      return
        Convert_Node_Class_Node_Attribute_To_Edge_Class_Id
        (Get_Node_Class_Id (Edge.Source_Node),
         Edge.Attribute);
   end Get_Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Node_Class_Id
     (Node : in Node_Id)
      return Node_Class_Id
   is
   begin
      return IML_Roots.Get_Class_ID (IML_Roots.IML_Root (Node.IML_Node));
   end Get_Node_Class_id;

   ---------------------------------------------------------------------------
   function Get_Edge_Tag
     (Edge : Edge_Id)
      return String
   is
      Attribute_Name : String :=
        Convert_Node_Attribute_Id_To_Name (Edge.Attribute);
   begin
      if Edge.Attribute_Element_Number = 0 then
         return Attribute_Name;
      else
         return Attribute_Name & "." &
           Integer'Image (Edge.Attribute_Element_Number);
      end if;
   end Get_Edge_Tag;

   ---------------------------------------------------------------------------
   function Get_Edge_Class_Tag
     (Edge_Class : Edge_Class_Id)
      return String
   is
   begin
      return
        Get_Node_Class_Tag (Edge_Class.Source_Node_Class) &
        Convert_Node_Attribute_Id_To_Name (Edge_Class.Source_Node_Attribute);
   end Get_Edge_Class_Tag;

   ----------------------------------------------------------------------------
   function Get_Incoming_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (Node.Incoming_Edges);
   end Get_Incoming_Edges;

   ----------------------------------------------------------------------------
   function Get_Outgoing_Edges
     (Node : in Node_Id)
      return Edge_Id_Set
   is
   begin
      return Edge_Id_Array_Routines.To_Set (Node.Outgoing_Edges);
   end Get_Outgoing_Edges;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Attribute_Node_Id_List_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_List
   is
      List : Node_Id_List;
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_Node_Id_List then
         raise Wrong_Attribute_Type;
      end if;

      List := Node_Id_Lists.Create;

      declare
         IML_List: IML_Reflection.List_Field
           := IML_Reflection.List_Field (Attribute.all);
         Iter    : IML_Reflection.List_Iterator;
         Target  : Storables.Storable;
         ResNode : Node_Id;
      begin
         Iter     := IML_List.Make_Iterator (Node.IML_Node);

         while IML_Reflection.More (Iter) loop
            IML_Reflection.Next (Iter, Target);

            ResNode := IML_Node_ID_Hashed_Mappings.Fetch
              (IML_Node_ID_Mapping,
               Storables.Get_Node_Id (Target) );

            Node_Id_Lists.Attach (List, ResNode);
         end loop;
      end;

      return List;
   end Get_Node_Attribute_Attribute_Node_Id_List_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Boolean_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Boolean
   is
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_Boolean then
         raise Wrong_Attribute_Type;
      end if;

      return IML_Reflection.Boolean_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_Boolean_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Class_Id
     (Node_Attribute : in Node_Attribute_Id)
      return Node_Attribute_Class_Id
   is
   begin
      --  Edges to other nodes
      if Node_Attribute.all in IML_Reflection.Edge_Field'Class then
         return Class_Node_Id;
      elsif Node_Attribute.all in IML_Reflection.List_Field'Class then
         return Class_Node_Id_List;
      elsif Node_Attribute.all in IML_Reflection.Set_Field'Class then
         return Class_Node_Id_Set;

         -- Buildin-Fields
      elsif Node_Attribute.all in IML_Reflection.SLoc_Field'Class then
         return Class_SLoc;
      elsif Node_Attribute.all in IML_Reflection.Boolean_Field'Class then
         return Class_Boolean;
      elsif Node_Attribute.all in IML_Reflection.Natural_Field'Class then
         return Class_Natural;
      elsif Node_Attribute.all in IML_Reflection.Enumerator_Field'Class then
         return Class_String_List;
      else
         My_Logger.Error (Node_Attribute.Name);
         My_Logger.Error ("Class for Node_Attribute could not be found");
         return Class_Natural;
      end if;
   end Get_Node_Attribute_Class_Id;

   ---------------------------------------------------------------------------
   function Get_Node_Attribute_Natural_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_Natural then
         raise Wrong_Attribute_Type;
      end if;

      return IML_Reflection.Natural_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_Natural_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Node_Id_Set_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id_Set
   is
      Set : Node_Id_Set;
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_Node_Id_Set then
         raise Wrong_Attribute_Type;
      end if;

      Set := Node_Id_Sets.Empty_Set;

      declare
         IML_Set : IML_Reflection.Set_Field :=
           IML_Reflection.Set_Field (Attribute.all);
         Iter    : IML_Reflection.Set_Iterator;
         Target  : Storables.Storable;
         ResNode : Node_Id;
      begin
         Iter := IML_Set.Make_Iterator (Node.IML_Node);

         while IML_Reflection.More (Iter) loop
            IML_Reflection.Next (Iter, Target);

            ResNode := IML_Node_ID_Hashed_Mappings.Fetch
              (IML_Node_ID_Mapping,
               Storables.Get_Node_Id (Target) );

            Node_Id_Sets.Insert (Set, ResNode);
         end loop;
      end;

      return Set;
   end Get_Node_Attribute_Node_Id_Set_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_Node_Id_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Node_Id
   is
      ResNode : Node_Id;
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_Node_Id then
         raise Wrong_Attribute_Type;
      end if;

      declare
          IML_Edge : IML_Reflection.Edge_Field
            := IML_Reflection.Edge_Field (Attribute.all);
          Target   : Storables.Storable;
       begin
          Target   := IML_Edge.Get_Target
            (IML_Roots.IML_Root (Node.IML_Node));
          if Storables."/=" (Target, null) then
             ResNode := IML_Node_ID_Hashed_Mappings.Fetch
               (IML_Node_ID_Mapping,
                Storables.Get_Node_Id (Target) );
          else
             My_Logger.Fatal ("Edge_Field with null target");
             raise Node_Does_Not_Exist;
          end if;
      end;

      return ResNode;
   end Get_Node_Attribute_Node_Id_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
     return SLocs.Sloc
   is
   begin
      if Get_Node_Attribute_Class_Id (Attribute) /= Class_SLoc then
         raise Wrong_Attribute_Type;
      end if;

      return IML_Reflection.SLoc_Field (Attribute.all).Get_Value
        (Node.IML_Node);
   end Get_Node_Attribute_SLoc_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Column_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return SLocs.Get_Column
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Column_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Filename_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      return SLocs.Get_Filename
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Filename_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Line_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return Natural
   is
   begin
      return SLocs.Get_Line
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Line_Value;

   ----------------------------------------------------------------------------
   function Get_Node_Attribute_SLoc_Path_Value
     (Node      : in     Node_Id;
      Attribute : in     Node_Attribute_Id)
      return String
   is
   begin
      return SLocs.Get_Path
        (Get_Node_Attribute_SLoc_Value (Node, Attribute));
   end Get_Node_Attribute_SLoc_Path_Value;

   --  function Get_Node_Attribute_String_Value
   --    (Node      : in     Node_Id;
   --     Attribute : in     Node_Attribute_Id)
   --     return String
   --  is
   --  begin
   --     return Get_Node_Attribute_String_Value (Node, Attribute);
   --  end Get_Node_Attribute_String_Value;

   ----------------------------------------------------------------------------
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

         when Class_Boolean =>
            if Get_Node_Attribute_Boolean_Value (Node, Attribute) then
               return "True";
            else
               return "False";
            end if;

         when Class_Natural =>
            return Natural'Image (Get_Node_Attribute_Natural_Value
                                  (Node, Attribute));
         when Class_Node_Id =>
            return Node_Id_Image (Get_Node_Attribute_Node_Id_Value
                                  (Node, Attribute));

         when Class_Node_Id_List =>
            return "";

         when Class_Node_Id_Set =>
            return "";

         when others =>
            My_Logger.Fatal ("Unknown Attribute-Class in " &
                             "Get_Node_Attribute_Value_As_String");
            return "";
      end case;
   end Get_Node_Attribute_Value_As_String;

   ----------------------------------------------------------------------------
   function Get_Root_Node
      return Node_Id
   is
      Root_Node : Storables.Storable;
      ResNode   : Node_Id;
   begin
      Root_Node := Storables.Storable
        (IML_Graphs.Get_Raw_Graph (IML_Graph));

      ResNode := IML_Node_ID_Hashed_Mappings.Fetch
        (IML_Node_ID_Mapping,
         Storables.Get_Node_Id (Root_Node) );

      return ResNode;
   end Get_Root_Node;

   ----------------------------------------------------------------------------
   function Get_Source_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Edge.Source_Node;
   end Get_Source_Node;

   ----------------------------------------------------------------------------
   function Get_Target_Node
     (Edge : in Edge_Id)
      return Node_Id
   is
   begin
      return Edge.Target_Node;
   end Get_Target_Node;

   ---------------------------------------------------------------------------
   function Get_Graph_Hash return Integer is
   begin
      return IML_Graphs.Hash (IML_Graph);
   end Get_Graph_Hash;

   ---------------------------------------------------------------------------
   function Hash_Edge_Class_Id (Key : in Edge_Class_Id) return Integer is

      Edge_Class_Id_Hash_Range_Size : constant := 29;

      package Edge_Class_Id_Hash_Functions is
         new Ptr_Normal_Hashs
        (T          => Edge_Class,
         T_Ptr      => Edge_Class_Id,
         Range_Size => Edge_Class_Id_Hash_Range_Size);

   begin
      return Edge_Class_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Edge_Class_Id;

   ---------------------------------------------------------------------------
   function Hash_Edge_Id (Key : in Edge_Id) return Integer is

      Edge_Id_Hash_Range_Size : constant := 1024047;

      package Edge_Id_Hash_Functions is
         new Ptr_Normal_Hashs
        (T          => Edge_Record,
         T_Ptr      => Edge_Id,
         Range_Size => Edge_Id_Hash_Range_Size);

   begin
      return Edge_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Edge_Id;

   ---------------------------------------------------------------------------
   function Hash_Node_Class_Id (Key : in Node_Class_Id) return Integer is
   begin
      return Node_Class_Id_Hash_Functions.Integer_Hash (Key);
   end Hash_Node_Class_Id;

   ---------------------------------------------------------------------------
   function Hash_Node_Id (Key : in Node_Id) return Integer is
   begin
      return IML_Node_IDs.Hash
        (Storables.Get_Node_ID (Key.IML_Node));
   end Hash_Node_Id;

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
     (Node_Class : in     Node_Class_Id)
     return Node_Attribute_Iterator
   is
      Iterator : Node_Attribute_Iterator;
   begin
      Iterator.Class := Node_Class;
      Iterator.CurrentIndex := Iterator.Class.Fields'First;
      return Iterator;
   end Make_Attribute_Iterator;

   ---------------------------------------------------------------------------
   function Make_Attribute_Iterator
     (Node     : in     Node_Id)
     return Node_Attribute_Iterator
   is
   begin
      return Make_Attribute_Iterator
        (IML_Roots.Get_Class_ID
         (IML_Roots.IML_Root (Node.IML_Node)));
   end Make_Attribute_Iterator;

   ---------------------------------------------------------------------------
   function More
     (Iterator : in Node_Attribute_Iterator)
     return Boolean
   is
   begin
      return (Iterator.CurrentIndex <= Iterator.Class.Fields'Last);
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iterator : in out Node_Attribute_Iterator;
      Info     :    out Node_Attribute_Id)
   is
   begin
      if not More (Iterator) then
         raise Constraint_Error;
         --  TBD: s/Contraint_Error/own_/
      end if;

      Info := Iterator.Class.Fields (Iterator.CurrentIndex);

      Iterator.CurrentIndex := Iterator.CurrentIndex + 1;
   end Next;

end Giant.Graph_Lib;
