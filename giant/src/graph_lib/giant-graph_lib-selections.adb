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
--  $RCSfile: giant-graph_lib-selections.adb,v $, $Revision: 1.13 $
--  $Author: koppor $
--  $Date: 2003/06/27 15:51:26 $

with Untagged_Ptr_Ops;

package body Giant.Graph_Lib.Selections is


   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Selection;
      Right : in Selection)
      return Boolean
   is

      package Compare is new Untagged_Ptr_Ops
        (T     => Selection_Record,
         T_Ptr => Selection);

   begin
      return Compare.Less (Left, Right);
   end "<";

   ---------------------------------------------------------------------------
   procedure Add_Edge
     (Selection_To_Modify : in out Selection;
      Edge                : in     Edge_Id)
   is
   begin
      Edge_Id_Sets.Insert
        (Selection_To_Modify.Edges,
         Edge);
   end Add_Edge;

   ---------------------------------------------------------------------------
   procedure Add_Edge_Set
     (Selection_To_Modify : in out Selection;
      Edge_Set            : in     Edge_Id_Set)
   is

      procedure Execute (Edge : in Edge_Id) is
      begin
         Add_Edge (Selection_To_Modify,
                   Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Edge_Set);
   end Add_Edge_Set;

   ---------------------------------------------------------------------------
   procedure Add_Node
     (Selection_To_Modify : in out Selection;
      Node                : in     Node_Id)
   is
   begin
      Node_Id_Sets.Insert
        (Selection_To_Modify.Nodes,
         Node);
   end Add_Node;

   ---------------------------------------------------------------------------
   procedure Add_Node_Set
     (Selection_To_Modify : in out Selection;
      Node_Set            : in     Node_Id_Set)
   is

      procedure Execute (Node : in Node_Id) is
      begin
         Add_Node (Selection_To_Modify,
                   Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Node_Set);
   end Add_Node_Set;

   ---------------------------------------------------------------------------
   function Clone
     (Selection_To_Clone : in Selection;
      Name_Of_Result     : in String)
      return Selection
   is
      Res : Selection;
   begin
      Res := new Selection_Record
        (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets.Copy (Selection_To_Clone.Edges);
      Res.Nodes := Node_Id_Sets.Copy (Selection_To_Clone.Nodes);
      return Res;
   end Clone;

   ---------------------------------------------------------------------------
   function Create
     (Name : in    String)
      return Selection
   is
      Res      : Selection;
   begin
      Res := new Selection_Record (Name_Length => Name'Length);
      Res.Name  := Name;
      Res.Edges := Edge_Id_Sets.Empty_Set;
      Res.Nodes := Node_Id_Sets.Empty_Set;
      return Res;
   end Create;

   ----------------------------------------------------------------------------
   procedure Destroy
     (Selection_To_Destroy : in out Selection)
   is

   begin
      Edge_Id_Sets.Destroy (Selection_To_Destroy.Edges);
      Node_Id_Sets.Destroy (Selection_To_Destroy.Nodes);

      Free_Selection (Selection_To_Destroy);
   end Destroy;

   ---------------------------------------------------------------------------
   function Get_All_Nodes
     (Sel : in Selection)
     return Node_Id_Set
   is
   begin
      return Sel.Nodes;
   end Get_All_Nodes;

   ---------------------------------------------------------------------------
   function Get_Edge_Count
     (Sel : in Selection)
     return Natural
   is
   begin
      return Edge_Id_Sets.Size (Sel.Edges);
   end Get_Edge_Count;

   ----------------------------------------------------------------------------
   function Get_Name
     (Selection_To_Read : in Selection)
      return String
   is
   begin
      return Selection_To_Read.Name;
   end Get_Name;

   ---------------------------------------------------------------------------
   function Get_Node_Count
     (Sel : in Selection)
     return Natural
   is
   begin
      return Node_Id_Sets.Size (Sel.Nodes);
   end Get_Node_Count;

   ----------------------------------------------------------------------------
   function Intersection
     (Left           : in Selection;
      Right          : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets."*" (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets."*" (Left.Nodes, Right.Nodes);
      return Res;
   end Intersection;

   ----------------------------------------------------------------------------
   procedure Remove_Edge
     (Selection_To_Modify : in out Selection;
      Edge                : in     Edge_Id)
   is
   begin
      begin
         Edge_Id_Sets.Remove (Selection_To_Modify.Edges,
                              Edge);
      exception
         when Edge_Id_Sets.No_Member =>
            raise Edge_Does_Not_Exist;
      end;
   end Remove_Edge;

   ----------------------------------------------------------------------------
   procedure Remove_Edge_Set
     (Selection_To_Modify : in out Selection;
      Edge_Set            : in     Edge_Id_Set)
   is

      procedure Execute (Edge : in Edge_Id) is
      begin
         Remove_Edge (Selection_To_Modify,
                      Edge);
      end Execute;

      procedure Apply is new Edge_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Edge_Set);
   end Remove_Edge_Set;

   ----------------------------------------------------------------------------
   procedure Remove_Node
     (Selection_To_Modify : in out Selection;
      Node                : in     Node_Id)
   is
   begin
      begin
         Node_Id_Sets.Remove (Selection_To_Modify.Nodes,
                              Node);
      exception
         when Node_Id_Sets.No_Member =>
            raise Node_Does_Not_Exist;
      end;
   end Remove_Node;

   ----------------------------------------------------------------------------
   procedure Remove_Node_Set
     (Selection_To_Modify : in out Selection;
      Node_Set            : in     Node_Id_Set)
   is

      procedure Execute (Node : in Node_Id) is
      begin
         Remove_Node (Selection_To_Modify,
                      Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Node_Set);
   end Remove_Node_Set;

   ----------------------------------------------------------------------------
   procedure Rename
     (Selection_To_Rename : in out Selection;
      New_Name            : in     String)
   is
      Res  : Selection;
   begin
      Res       := new Selection_Record (Name_Length => New_Name'Length);
      Res.Name  := New_Name;
      Res.Edges := Selection_To_Rename.Edges;
      Res.Nodes := Selection_To_Rename.Nodes;

      Free_Selection (Selection_To_Rename);
      Selection_To_Rename := Res;
   end Rename;

   ----------------------------------------------------------------------------
   procedure Selection_Read
     (Stream : in      Bauhaus_Io.In_Stream_Type;
      Sel    :     out Selection)
   is
      Len    : Natural;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Node_Id
        (Stream : in     Bauhaus_Io.In_Stream_Type;
         Node   :    out Node_Id)
      is
         Len : Natural;
      begin
         Bauhaus_Io.Read_Natural (Stream, Len);

         declare
            Image : String (1..Len);
         begin
            Bauhaus_Io.Read_String (Stream, Image);
            Node := Node_Id_Value (Image);

            --  TBD: an exception could be risen here
         end;
      end Read_Node_Id;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Node_Id_Set is new Node_Id_Sets.Read_Set
        (Read_Element => Read_Node_Id);

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Edge_Id
        (Stream : in     Bauhaus_Io.In_Stream_Type;
         Edge   :    out Edge_Id)
      is
         Edge_Internal_Id         : Positive;
      begin
         Bauhaus_Io.Read_Natural (Stream, Edge_Internal_Id);
         Edge := All_Edges (Edge_Internal_Id);
      end Read_Edge_Id;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Read_Edge_Id_Set is new Edge_Id_Sets.Read_Set
        (Read_Element => Read_Edge_Id);

   begin
      Bauhaus_Io.Read_Natural (Stream, Len);
      Sel := new Selection_Record (Name_Length => Len);
      Bauhaus_Io.Read_String (Stream, Sel.Name);

      -- TBD: FIX: graph_lib Read_Edge_Id_Set (Stream, Sel.Edges);
      Read_Node_Id_Set (Stream, Sel.Nodes);
   end Selection_Read;

   ----------------------------------------------------------------------------
   procedure Selection_Write
     (Stream : in Bauhaus_Io.Out_Stream_Type;
      Sel    : in Selection)
   is

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Node_Id
        (Stream : in Bauhaus_Io.Out_Stream_Type;
         Node   : in Node_Id)
      is
         Image : String := Node_Id_Image (Node);
      begin
         Bauhaus_Io.Write_Natural (Stream, Image'Length);
         Bauhaus_Io.Write_String  (Stream, Image);
      end Write_Node_Id;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Node_Id_Set is new Node_Id_Sets.Write_Set
        (Write_Element => Write_Node_Id);

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Edge_Id
        (Stream : in Bauhaus_Io.Out_Stream_Type;
         Edge   : in Edge_Id)
      is
      begin
         Bauhaus_Io.Write_Natural (Stream, Edge.Internal_Id);
      end Write_Edge_Id;

      ----------------------------------------------------------------------
      --  Is implemented here, since it is used only once
      --    if it will be used more, please put it in giant-graph_lib.ads
      procedure Write_Edge_Id_Set is new Edge_Id_Sets.Write_Set
        (Write_Element => Write_Edge_Id);

   begin
      Bauhaus_Io.Write_Natural (Stream, Sel.Name'Length);
      Bauhaus_Io.Write_String  (Stream, Sel.Name);
      --  TBD: FIXME: Write_Edge_Id_Set (Stream, Sel.Edges);
      Write_Node_Id_Set (Stream, Sel.Nodes);
   end Selection_Write;

   ----------------------------------------------------------------------------
   function Symetric_Difference
     (Left           : in Selection;
      Right          : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets.Symmetric_Difference (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets.Symmetric_Difference (Left.Nodes, Right.Nodes);
      return Res;
   end Symetric_Difference;

   ----------------------------------------------------------------------------
   function Union
     (Left          : in Selection;
      Right         : in Selection;
      Name_Of_Result : in String)
      return Selection
   is
      Res      : Selection;
   begin
      Res       := new Selection_Record (Name_Length => Name_Of_Result'Length);
      Res.Name  := Name_Of_Result;
      Res.Edges := Edge_Id_Sets."+" (Left.Edges, Right.Edges);
      Res.Nodes := Node_Id_Sets."+" (Left.Nodes, Right.Nodes);
      return Res;
   end Union;

end Giant.Graph_Lib.Selections;
