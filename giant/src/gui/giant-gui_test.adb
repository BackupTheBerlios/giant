------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- $RCSfile: giant-gui_test.adb,v $, $Revision: 1.4 $
-- $Author: squig $
-- $Date: 2003/05/31 19:23:40 $
--
with Gtk.Main;

with Ada.Text_Io; use Ada.Text_Io;

with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Main_Window;
with Giant.Gsl_Dialog;
with Giant.Main_Window;
with Giant.Logger;
--with Config;

procedure Giant.Gui_Test
is
   My_Gsl_Dialog : Gsl_Dialog.Gsl_Dialog_Access;
begin
   Default_Logger.Init;
   --Config.Initialize_Config_Data ("/etc/giant/giantrc", ".giantrc");

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

--     Default_Dialog.Show_Error ("An Error has occured!");

--     Put_Line("Input:" & Default_Dialog.Show_Input ("Insert Text:"));

     Gsl_Dialog.Create (My_Gsl_Dialog);
     Gsl_Dialog.Show_All (My_Gsl_Dialog);

--     Main_Window.Show;

   Gtk.Main.Main;

   Default_Logger.Close;
end Giant.Gui_Test;

