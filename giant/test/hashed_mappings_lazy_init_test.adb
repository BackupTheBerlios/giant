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
--  $RCSfile: hashed_mappings_lazy_init_test.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Logger;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);

package body Hashed_Mappings_Lazy_Init_Test is

   package Logger is new Giant.Logger("Hashed_Mappings.Test");
      
   function H_Int (Item : in Integer) return Integer is   
   begin  
     return Item;   
   end H_Int;
         
   package Test_Hash_Pkg is new Hashed_Mappings
     (Key_Type   => Integer,
      Equal      => "=",
      Hash       => H_Int,
      Value_Type => Integer);
            

   ---------------------------------------------------------------------------
   procedure Test_Init
      (R : in out AUnit.Test_Cases.Test_Case'Class) is

      Test_Map : Test_Hash_Pkg.Mapping;
   begin
   

      Test_Map  := Test_Hash_Pkg.Create (26);   
   end Test_Init;  



   ---------------------------------------------------------------------------
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Hashed_Mappings - Lazy Init Tests");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Test_Init'Access, 
         "Test_Hash"); 
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;                      
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      null;
   end Tear_Down;

end Hashed_Mappings_Lazy_Init_Test;
