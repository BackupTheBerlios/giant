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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/06/15 12:45:42 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with Giant.Controller;
with Giant.Default_Logger;
with Giant.Logger;

procedure Giant.Main
is
   package Logger is new Giant.Logger("giant.main");
   --Current_Project : Projects.Project_Access;
begin
   Giant.Default_Logger.Init;

   Logger.Debug ("initializing gtk");

--     Current_Project := Projects.Create_Empty_Project
--       (Valid_Names.To_Standard_Name ("GiantTest"), "testdata",
--        "../src/vis_test/rfg_examp.iml", 0);

   Controller.Show;

   Logger.Debug ("closing giant");

   Giant.Default_Logger.Close;
end Giant.Main;
