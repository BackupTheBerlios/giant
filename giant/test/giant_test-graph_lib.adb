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
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-graph_lib.adb,v $, $Revision: 1.2 $
--  $Author: koppor $
--  $Date: 2003/06/12 14:24:42 $
--
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with Giant.Graph_Lib;

procedure Giant_Test.Graph_Lib is
begin
   Put_Line ("Bad thing - you'll se an exception NOW:");
   Giant.Graph_Lib.Create ("/home/stsopra/giant/graphs/rfg_examp.iml");
   Giant.Graph_Lib.Destroy;
end Giant_Test.Graph_Lib;


