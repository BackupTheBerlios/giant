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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.20 $
--  $Author: squig $
--  $Date: 2003/06/25 18:59:59 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with GNAT.OS_Lib;

with Giant.Config_Settings;
with Giant.Config.Global_Data;
with Giant.Config.Vis_Styles;
with Giant.Controller;
with Giant.Default_Logger;
with Giant.File_Management;
with Giant.Graph_Lib;
with Giant.Logger;

procedure Giant.Main
is
   package Logger is new Giant.Logger("giant.main");

   Config_Filename : String
     := File_Management.Get_User_Config_Path & "settings.xml";
begin
   Default_Logger.Init;

   --  load config settings
   Config_Settings.Initialize_Config_Settings
     ("test/resources/global_config.xml", Config_Filename);

   Config.Global_Data.Initialize_Config_Data;

   Giant.Graph_Lib.Initialize;

   Config.Vis_Styles.Initialize_Config_Vis_Styles
     (Resources_Root_Dir     =>
        Config.Global_Data.Get_Resources_Directory,
      GIANT_VIS_Directory    => "",
      User_Vis_Directory     => "",
      Default_Vis_Style_File =>
        "test/resources/vis_styles/only_defaults_giant_vis_style.xml");

   -- FIX: remove the following lines
   declare
   begin
--        declare
--        begin
--           File_Management.Delete_File ("test/resources/GiantTest.xml");
--        exception
--           when others =>
--              null;
--        end;
      Controller.Open_Project ("test/resources/test.xml");
   exception
      when others =>
         null;
   end;

   Logger.Debug ("starting giant");

   Controller.Show_Gui;

   Logger.Debug ("closing giant");

   Giant.Graph_Lib.Destroy;

   --  store config settings
   Logger.Debug ("storing config settings: " & Config_Filename);
   Config_Settings.Store_User_Config_File (Config_Filename);


   Giant.Default_Logger.Close;
end Giant.Main;
