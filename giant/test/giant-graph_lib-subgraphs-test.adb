------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-graph_lib-subgraphs-test.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Lib.Subgraphs.Test is

   -------------------------------------
   --  Global variables and routines  --
   -------------------------------------

   package Logger is new Giant.Logger("giant.graph_lib.subgraphs.test");

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   --  Init of the graph
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load ("resources/rfg_examp.iml");
   end Init;

   procedure Test_Rename_Duplicate
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      My_Subgraph : Subgraph;
      Cloned_Subgraph : Subgraph;
   begin
      My_Subgraph := Create ("Test Graph");

      Graph_Lib.Subgraphs.Add_Edge_Set (My_Subgraph, Graph_Lib.Get_All_Edges);
      Graph_Lib.Subgraphs.Add_Node_Set (My_Subgraph, Graph_Lib.Get_All_Nodes);

      Rename (My_Subgraph, "Renamed Graph");
      Cloned_Subgraph := Clone (My_Subgraph, "Cloned Renamed Graph");

      Destroy (My_Subgraph);
      Destroy (Cloned_Subgraph);
   end Test_Rename_Duplicate;

   ---------------------------------------------------------------------------
   --  Works only with rfg_examp.iml
   procedure Test_Selection_To_Subgraph
     (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Selection : Selections.Selection;
      Node          : Graph_Lib.Node_Id;

      The_Graph     : Subgraph;
   begin
      The_Selection := Selections.Create ("");

      --  Node with all outgoing edges
      Node := Graph_Lib.Node_Id_Value ("3");
      Selections.Add_Node (The_Selection, Node);
      declare
         Outgoing_Edges : Graph_Lib.Edge_Id_Array :=
           Graph_Lib.Get_Outgoing_Edges (Node);
      begin
         for I in Outgoing_Edges'Range loop
            Selections.Add_Edge (The_Selection, Outgoing_Edges (I));
         end loop;
      end;

      Logger.Warn (Integer'Image (Selections.Get_Edge_Count (The_Selection)));

      -- add one target node
      Selections.Add_Node (The_Selection, Graph_Lib.Node_Id_Value ("35"));

      The_Graph := Create ("", The_Selection);

      Assert (Get_Edge_Count (The_Graph) = 1,
              "Graph is not a real graph");

      Selections.Destroy (The_Selection);
      Destroy (The_Graph);
   end Test_Selection_To_Subgraph;

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
      Register_Routine (T, Test_Rename_Duplicate'Access, "Rename_Duplicate");
      Register_Routine (T, Test_Selection_To_Subgraph'Access,
                        "Selection to Subgraph");
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

end Giant.Graph_Lib.Subgraphs.Test;
