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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/06/18 15:16:26 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with Giant.Config;
with Giant.Config.Vis_Styles;
with Giant.Controller;
with Giant.Default_Logger;
with Giant.Logger;

procedure Giant.Main
is
   package Logger is new Giant.Logger("giant.main");
   --Current_Project : Projects.Project_Access;
begin
   Giant.Default_Logger.Init;

   -- read config
--     Config.Vis_Styles.Initialize_Config_Vis_Styles
--       ("", "", "test/resources/giant_vis_style.xml");

   Controller.Create_Project
     ("GiantTest", "test/resources", "test/resources/rfg_examp.iml");

   Logger.Debug ("initializing gtk");

   Controller.Show_Gui;

   Logger.Debug ("closing giant");

   Giant.Default_Logger.Close;
end Giant.Main;
