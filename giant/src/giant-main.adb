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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.22 $
--  $Author: squig $
--  $Date: 2003/06/27 16:58:06 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
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

   Version : constant String := "20030627";

   Config_Filename : constant String
     := File_Management.Get_User_Config_Path & "settings.xml";

   procedure Put_Help
   is
   begin
      Ada.Text_IO.Put_Line
        ("usage: giant [(-n project -g graph | -l project) -e script | -h | -v]");
   end;

   procedure Parse_Arguments
   is
      use Ada.Strings.Unbounded;

      Project_Filename : String_Access := new String' ("");
      Graph_Filename   : String_Access := new String' ("");
      Script_Filename  : String_Access := new String' ("");
   begin
      GNAT.Command_Line.Initialize_Option_Scan;

      loop
         case GNAT.Command_Line.Getopt
           ("-newproject: -graph: -loadproject: -execute: -version -help")
         is
           --  long option names
           when '-' =>
              case GNAT.Command_Line.Full_Switch
                (GNAT.Command_Line.Full_Switch'First + 1) is
                when 'n' =>
                   --  "newproject"
                   Free (Project_Filename);
                   Project_Filename := new String'(GNAT.Command_Line.Parameter);
                when 'e' =>
                   --  "execute"
                   Free (Script_Filename);
                   Script_Filename := new String'(GNAT.Command_Line.Parameter);
                when 'g' =>
                   --  "graph"
                   Free (Graph_Filename);
                   Graph_Filename := new String'(GNAT.Command_Line.Parameter);
                when 'h' =>
                   --  "help"
                   Put_Help;
                   GNAT.OS_Lib.OS_Exit (0);
                when 'v' =>
                   --  "version"
                   Ada.Text_IO.Put_Line ("version: " & Version);
                   GNAT.OS_Lib.OS_Exit (0);
               when others =>
                  null;
              end case;
           when ASCII.Nul =>
              exit;
           when others =>
              null;
         end case;
      end loop;
   end Parse_Arguments;

   procedure New_Test_Project
   is
   begin
      begin
         File_Management.Delete_File ("test/resources/test.xml");
      exception
         when others =>
            null;
      end;
      Controller.Create_Project ("test/resources/test.xml",
                                 "test/resources/rfg_examp.iml");
   exception
      when others =>
         null;
   end New_Test_Project;

   procedure Open_Test_Project
   is
   begin
      Controller.Open_Project ("test/resources/test.xml");
   exception
     when others =>
        null;
   end Open_Test_Project;

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

   Logger.Debug ("parsing command line arguments");
   Parse_Arguments;

   New_Test_Project;

   Logger.Debug ("starting giant");

   Controller.Show_Gui;

   Logger.Debug ("closing giant");

   Giant.Graph_Lib.Destroy;

   --  store config settings
   Logger.Debug ("storing config settings: " & Config_Filename);
   Config_Settings.Store_User_Config_File (Config_Filename);


   Giant.Default_Logger.Close;
end Giant.Main;
