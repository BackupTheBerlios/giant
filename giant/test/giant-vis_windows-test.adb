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
--  $RCSfile: giant-vis_windows-test.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/16 21:48:31 $
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Vis_Windows;
with Giant.Default_Logger;
use type Giant.Valid_Names.Standard_Name;

package body Giant.Vis_Windows.Test is

   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Window : Visual_Window_Access;
   begin
      Window := Vis_Windows.Create_New (Valid_Names.To_Standard_Name ("Name"));

      Assert (Vis_Windows.Get_Name (Window) = Valid_Names.To_Standard_Name ("Name"), "Name");
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Vis_Windows");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Init");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Default_Logger.Init;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      Default_Logger.Close;
   end Tear_Down;

end Giant.Vis_Windows.Test;
