------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und ‹bersetzerbau, University of Stuttgart for
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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-tree_layouts-test.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Config;
with Giant.Config.Class_Sets;
with Giant.Config.Vis_Styles;
with Giant.Graph_Widgets;
with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

with Gtk.Main;

package body Giant.Tree_Layouts.Test is

   type Graph_Data_Record (Len : Natural) is record
      Filename   : String (1..Len);
      Edge_Count : Natural;
      Node_Count : Natural;
   end record;
   type Graph_Data is access constant Graph_Data_Record;

   Rfg_Example : aliased constant Graph_Data_Record :=
     (Len => 23,
      Filename => "resources/rfg_examp.iml",
      Edge_Count => 631,  --  with all: 646
      Node_Count => 202); --  with all: 216

   Graphs : constant array (1..1) of Graph_Data :=
     ( 1 => Rfg_Example'Access );

   --------------------------------------------------------------------------
   --  Index in Graphs of graph to test with
   --  TBD: extention, that all graphs are tested with the same test cases
   Test_Graph_Number : constant := 1;

   --------------------------------------------------------------------------
   package Logger is new Giant.Logger("T:Tree-Layouts");

   --------------------------------------------------------------------------
   Widget : Graph_Widgets.Graph_Widget;

   -----------------
   --  Testcases  --
   -----------------

   ---------------------------------------------------------------------------
   procedure Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Gtk.Main.Init;

      Giant.Graph_Lib.Initialize;
      Giant.Graph_Lib.Load (Graphs (Test_Graph_Number).FileName);

      Giant.Config.Vis_Styles.Initialize_Config_Vis_Styles
        ("",
         "",
         "",
         "resources/vis_styles/vis_styles_test_set_1/"
         & "test_vis_style_2.xml");

      Graph_Widgets.Create (Widget);
   end Init;

   procedure Small_Tree (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Sel    : Graph_Lib.Selections.Selection;
      Lock   : Graph_Widgets.Lock_Type;
      Layout : Tree_Layouts.Tree_Layout;
   begin
      --  Generate Selection
      Sel := Graph_Lib.Selections.Create ("");
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("3"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("10"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("68"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("70"));
      Graph_Lib.Selections.Add_Node (Sel, Graph_Lib.Node_Id_Value ("123"));

      Graph_Widgets.Insert_Selection (Widget, Sel, Lock);

      Layout := Tree_Layouts.Initialize
        (Widget              => Widget,
         Widget_Lock         => Lock,
         Selection_To_Layout => Sel,
         Target_Position     => Vis.Logic.Combine_Vector (0.0, 0.0),
         Root_Node           => Graph_Lib.Node_Id_Value ("3"),
         --  q&d -- a destroy of this empty class_set should be called
         Meta_Class_Set_To_Layout =>
           Config.Class_Sets.Build
         (Config.Class_Sets.Class_Sets_Lists.Create),
         Process_Edges_Reverse => False);

      Evolutions.Start_Calculation_Blocked (Layout);

      -- Finish test
      Graph_Lib.Selections.Destroy (Sel);
   end Small_Tree;

   procedure Done (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      --  TBD: destroy widget

      Giant.Graph_Lib.Unload;
      Giant.Graph_Lib.Destroy;
   end Done;

   --------------------------------
   --  Routines from AUnit-Test  --
   --------------------------------

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Layouts-Tree_Layouts");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Init'Access, "Init");
      Register_Routine (T, Small_Tree'Access, "Small Tree");
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

end Giant.Tree_Layouts.Test;
