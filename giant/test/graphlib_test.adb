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
--  $RCSfile: graphlib_test.adb,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/06/23 15:07:30 $
--

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Giant.Graph_Lib.Test;
with Giant.Default_Logger;

procedure Graphlib_Test is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Giant.Graph_Lib.Test.Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Giant.Default_Logger.Init;
   Giant.Default_Logger.Debug ("Starting Test...");

   Run;

   Giant.Default_Logger.Close;
end Graphlib_Test;
