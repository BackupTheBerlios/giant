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
--  $RCSfile: hashed_mappings_lazy_init_test.adb,v $, $Revision: 1.1 $
--  $Author: schwiemn $
--  $Date: 2003/07/21 18:40:06 $
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
