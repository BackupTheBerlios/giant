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
--  $RCSfile: hashed_mappings_test.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Hashed_Mappings;
with Lists;
pragma Elaborate_All (Lists);
with Storables;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Hashed_Mappings_Test is

   package Logger is new Giant.Logger("hashed_mappings_test");

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

   package Bucket_Pkg is new Lists (Node_Access, "=");

   Primes : constant array (1..30) of Natural
     := ( 1 => 3,
          2 => 7,
          3 => 17,
          4 => 31,
          5 => 61,
          6 => 127,
          7 => 257,
          8 => 509,
          9 => 1021,
         10 => 2053,
         11 => 4093,
         12 => 8191,
         13 => 16381,
         14 => 32771,
         15 => 65537,
         16 => 131071,
         17 => 262139,
         18 => 524287,
         19 => 1048573,
         20 => 2097143,
         21 => 4194301,
         22 => 8388617,
         23 => 16777213,
         24 => 33554467,
         25 => 67108859,
         26 => 134217757,
         27 => 268435459,
         28 => 536870909,
         29 => 1073741827,
         30 => 2147483647); -- Natural'LAST

   type Bucket_Array is array (Integer range <>) of Bucket_Pkg.List;

   type Mapping_Rec (Max_Bucket : Natural) is record
      Size    : Natural;
      Index   : Natural; -- prime index
      Buckets : Bucket_Array (0 .. Max_Bucket);
   end record;

   type Mapping is access Mapping_Rec;

   procedure Test_Create (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Big_Mapping : Mapping;
   begin
      Big_Mapping
        := new Mapping_Rec(Primes (25));
--        Assert (Bucket_Pkg.IsEmpty (Big_Mapping.Buckets (0)),
--                "Created");
      Logger.debug ("max bucket : " & Natural'Image (Big_Mapping.Max_Bucket));
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Hashed_Mappings");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Create'Access, "Create");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Hashed_Mappings_Test;
