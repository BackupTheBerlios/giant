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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-file_management-test.adb,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:57 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with Ada.Command_Line;

with String_Lists;

with GNAT.Directory_Operations;

with Giant.Config;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.File_Management.Test is

   package Logger is new Giant.Logger("giant.file_management.test");

   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Exec_Path : String := File_Management.Return_Dir_Path_For_File_Path
        (Ada.Command_Line.Command_Name);
   begin
--        GNAT.Directory_Operations.Get_Current_Dir;
--        Giant.File_Management.Set_Currunt_Working_Dir_To_Exec_Dir;
--        GNAT.Directory_Operations.Get_Current_Dir;

      Assert (Get_Absolute_Path_To_Directory_From_Relative
              (GNAT.Directory_Operations.Get_Current_Dir,
               "resources")
              = GNAT.Directory_Operations.Get_Current_Dir
              & "resources/",
              "Get_Absolute_Path_To_Directory_From_Relative");

      Assert (Get_Absolute_Path_To_File_From_Relative
              (GNAT.Directory_Operations.Get_Current_Dir,
               "resources/rfg_examp.iml")
              = GNAT.Directory_Operations.Get_Current_Dir
              & "resources/rfg_examp.iml",
              "Get_Absolute_Path_To_File_From_Relative");

--        Assert (Exec_Path = GNAT.Directory_Operations.Get_Current_Dir,
--                "Exec_Path = Current_Dir");
   end;

   --------------------------------------------------------------------------
   procedure Test_Filter  (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      List : String_Lists.List;
      Iter : String_Lists.ListIter;
      S    : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List :=
        File_Management.Get_Filtered_Files_From_Directory
        (Path_To_Dir   => "resources/class_sets/",
         Filter        => True,
         Filter_String => ".xml");

      Iter := String_Lists.MakeListIter (List);
      while String_Lists.More (Iter) loop
         String_Lists.Next (Iter, S);
         Logger.Info (Ada.Strings.Unbounded.To_String (S));
      end loop;

      Assert (String_Lists.Length (List)=1,
              "find exactly one file");

      String_Lists.Destroy (List);
   end Test_Filter;

   --------------------------------------------------------------------------
   procedure Test_Names (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (Calculate_Name_For_File ("./test/my_file.xml") = "my_file",
              "Calculate_Name_For_File: ./test/my_file.xml");
      Assert (Calculate_Name_For_File ("a.data") = "a",
              "Calculate_Name_For_File: a.data");
      Assert (Calculate_Name_For_File ("./../../data") = "data",
              "Calculate_Name_For_File: ./../../data");
      Assert (Calculate_Name_For_File ("") = "",
              "Calculate_Name_For_File: Empty String");
      Assert (Calculate_Name_For_File (".") = "",
              "Calculate_Name_For_File: .");

      Logger.Debug ("resources/GiantTest.xml: "
                    & Return_Dir_Path_For_File_Path ("resources/GiantTest.xml"));
      Logger.Debug ("/tmp/GiantTest.xml: "
                    & Return_Dir_Path_For_File_Path ("/tmp/GiantTest.xml"));

      Assert (Return_Dir_Path_For_File_Path ("resources/test") = "resources/",
              "Return_Dir_Path_For_File_Path: resources/test");
      Assert (Return_Dir_Path_For_File_Path ("/tmp") = "/",
              "Return_Dir_Path_For_File_Path: /tmp");
   end Test_Names;

   procedure Test_Substitute (R : in out AUnit.Test_Cases.Test_Case'Class) is

   begin

      Logger.Debug ("Substitute Soll: ""Life is Life"" - IST: """
                    & File_Management.Substitute_Sub_Strings
                        (Source => "%A% is %A%",
                         Needle => "%A%",
                         Fork   => "Life")
                    & """");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "%A% is %A%",
                  Needle => "%A%",
                  Fork   => "Life") = "Life is Life",
              "String: ""Required Result: Life is Life""");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "Hallo",
                  Needle => "%A%",
                  Fork   => "Life") = "Hallo",
              "String: ""Required Result: Hallo""");

     Assert (File_Management.Substitute_Sub_Strings
                 (Source => "zu kurz",
                  Needle => "%aaaaaaaDu%",
                  Fork   => "Life") = "zu kurz",
              "String: ""Required Result: Du""");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "%",
                  Needle => "%",
                  Fork   => "Winner") = "Winner",
              "String: ""Required Result: Winner""");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "%%%%%",
                  Needle => "%",
                  Fork   => "Winner") = "WinnerWinnerWinnerWinnerWinner",
              "String: ""Required Result: WinnerWinnerWinnerWinnerWinner""");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "The  Big  Boss",
                  Needle => " Big ",
                  Fork   => "Small") = "The Small Boss",
              "String: ""Required Result: The Small Boss""");

      Assert (File_Management.Substitute_Sub_Strings
                 (Source => "The Big Boss",
                  Needle => " big ",
                  Fork   => "Small") = "The Big Boss",
              "String: ""Required Result: The Big Boss""");
   end Test_Substitute;

   procedure Test_Editor_Call (R : in out AUnit.Test_Cases.Test_Case'Class) is

   begin

     File_Management.Execute_External_Editor
       (Command  => "/usr/bin/emacs +%l:%c %f",
        Filename => "./resources/global_config.xml",
        Line     => 10,
        Column   => 5);

   end Test_Editor_Call;


   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("File_Management");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access,        "Init");
      Register_Routine (T, Test_Names'Access,       "Names");
      Register_Routine (T, Test_Substitute'Access,  "Substitute");
      Register_Routine (T, Test_Filter'Access,      "Filter");

      -- spawns an external editor - not useful for automated regression tests
      --Register_Routine (T, Test_Editor_Call'Access, "Editor_Call");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Giant.File_Management.Test;
