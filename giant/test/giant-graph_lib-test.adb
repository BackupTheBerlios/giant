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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-graph_lib-test.adb,v $, $Revision: 1.8 $
--  $Author: koppor $
--  $Date: 2003/07/01 16:38:47 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Logger;

package body Giant.Graph_Lib.Test is

   --------------------------------------------------------------------------
   --  Parameters for the graph to use for testing

     IML_Filename   : constant String :=
       "/home/stsopra/giant/graphs/httpd.iml";
     IML_Edge_Count : constant Integer := 2316410;
     IML_Node_Count : constant Integer := 797777;

--     IML_Filename   : constant String :=
--       "/home/stsopra/giant/graphs/wget.iml";
--     IML_Edge_Count : constant Integer := 1274794;
--     IML_Node_Count : constant Integer := 472583;

--     IML_Filename   : constant String :=
--       "/home/stsopra/giant/graphs/concept_analysis.iml";
--     IML_Edge_Count : constant Integer := 156072;
--     IML_Node_Count : constant Integer := 646;

--     IML_Filename   : constant String  := "resources/rfg_examp.iml";
--     IML_Edge_Count : constant Integer := 646;
--     IML_Node_Count : constant Integer := 216;

   -------------------------------------
   --  Global variables and routines  --
   -------------------------------------

   package Logger is new Giant.Logger("T:graph_lib");

   Root : Node_Id;

   procedure Output_Attributes (Node : in Node_Id) is
      Iter : Node_Attribute_Iterator;
      Attr : Node_Attribute_Id;
   begin
      Iter := Make_Attribute_Iterator (Node);
      while More (Iter) loop
         Next (Iter, Attr);

         Logger.Debug
           ("Attribute " & Convert_Node_Attribute_Id_To_Name (Attr));

         case Get_Node_Attribute_Class_Id (Attr) is
            when Class_Node_Id =>
               null;
            when Class_SLoc =>
               null;
            when others =>
               null;
         end case;

      end loop;
   end Output_Attributes;

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   --  Init of the graph
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load (IML_FileName);

      Root := Get_Root_Node;
   end Init;

   procedure Output_RootNode (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Logger.Debug ("Root Node with ID: " & Node_Id_Image (Root));
      Output_Attributes (Root);
   end Output_RootNode;

   procedure Check_Counts (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  check count directly
      --  n/a, since edges are not referenced directly

      Logger.Debug ("Edges: " & Integer'Image (Get_Edge_Count));
      Logger.Debug ("Nodes: " & Integer'Image (Get_Node_Count));

      Assert (Get_Edge_Count = IML_Edge_Count, "Edge_Count");
      Assert (Get_Node_Count = IML_Node_Count, "Node_Count");

      --  check count indirectly
      declare
         Set : Edge_Id_Set;
      begin
         Set := Giant.Graph_Lib.Get_All_Edges;
         Assert (Edge_Id_Sets.Size (Set) = IML_Edge_Count, "All_Edges.Size");
         Edge_Id_Sets.Destroy (Set);
      end;

      --  check count directly
      Assert (IML_Node_ID_Hashed_Mappings.Size (IML_Node_ID_Mapping) =
              IML_Node_Count,
              "All_Nodes.Size (via Mapping)");

      --  check count indirectly
      declare
         Set : Node_Id_Set;
      begin
         Set := Giant.Graph_Lib.Get_All_Nodes;
         Logger.Debug ("Size: " & Integer'Image (Node_Id_Sets.Size (Set)));
         Assert (Node_Id_Sets.Size (Set) = IML_Node_Count, "All_Nodes.Size");
         Node_Id_Sets.Destroy (Set);
      end;

   end Check_Counts;

   procedure Edge_Set_Test (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      null;
   end Edge_Set_Test;

   ---------------------------------------------------------------------------
   --  Could be implemented more efficient
   --    a) traverse All_Nodes "by hand"
   --         don't use function Get_All_Nodes, but travel thourgh the hashmap
   --         directly
   procedure All_Edges_Test (R : in out AUnit.Test_Cases.Test_Case'Class)
   is

      function Get_All_Edges_Indirectly return Edge_Id_Set
      is
         All_Nodes : Node_Id_Sets.Set;
         Iter      : Node_Id_Sets.Iterator;
         Cur_Node  : Node_Id;
         Result    : Edge_Id_Sets.Set;
      begin
         Result := Edge_Id_Sets.Empty_Set;

         All_Nodes := Get_All_Nodes;

         Iter := Node_Id_Sets.Make_Iterator (All_Nodes);

         while Node_Id_Sets.More (Iter) loop
            Node_Id_Sets.Next (Iter, Cur_Node);

            Edge_Id_Sets.Union (Result, Get_Outgoing_Edges (Cur_Node));
         end loop;

         Node_Id_Sets.Destroy (All_Nodes);

         return Result;
      end Get_All_Edges_Indirectly;


      All_Edges_Indirectly : Edge_Id_Sets.Set;
      All_Edges_Directly : Edge_Id_Sets.Set;

   begin
      All_Edges_Indirectly := Get_All_Edges_Indirectly;
      All_Edges_Directly   := Get_All_Edges;

      -- FIXME: Assert ( "=", 1, 2);

      Edge_Id_Sets.Destroy (All_Edges_Directly);
      Edge_Id_Sets.Destroy (All_Edges_Indirectly);
   end All_Edges_Test;

   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Unload;
      Giant.Graph_Lib.Destroy;
   end Done;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Graph_Lib");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Init'Access, "Init");
      Register_Routine (T, Output_RootNode'Access, "Output_RootNode");
      Register_Routine (T, Check_Counts'Access, "Check_Counts");
      Register_Routine (T, Edge_Set_Test'Access, "Edge_Set_Test");
      Register_Routine (T, All_Edges_Test'Access, "All Edges Test");
      Register_Routine (T, Done'Access, "Done");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.Graph_Lib.Test;
