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
--  $RCSfile: giant-valid_names-test.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/16 15:41:09 $
--
------------------------------------------------------------------------------
--
--  Provides an aunit test.
--

with Ada.Strings.Unbounded;

with AUnit.Test_Cases;

package Giant.Valid_Names.Test is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   
   --  Register routines to be run:
   procedure Register_Tests (T : in out Test_Case);
   
   --  Provide name identifying the test case:
   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;
   
   --  Preparation performed before each routine:
   procedure Set_Up (T : in out Test_Case);
   
   --  Cleanup performed after each routine:
   procedure Tear_Down (T :  in out Test_Case);
   
end Giant.Valid_Names.Test;
