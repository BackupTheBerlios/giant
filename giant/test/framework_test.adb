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
--  $RCSfile: framework_test.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/16 15:27:58 $
--

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Giant.Config.Test;
with Giant.Default_Logger.Test;
with Giant.File_Management.Test;
with Giant.Graph_Lib.Test;
with Giant.Vis_Windows.Test;

procedure Framework_Test is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;
   begin
      Add_Test (Result, new Giant.Config.Test.Test_Case);
      Add_Test (Result, new Giant.Default_Logger.Test.Test_Case);
      Add_Test (Result, new Giant.File_Management.Test.Test_Case);
      Add_Test (Result, new Giant.Graph_Lib.Test.Test_Case);
      Add_Test (Result, new Giant.Vis_Windows.Test.Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Run;
end Framework_Test;
