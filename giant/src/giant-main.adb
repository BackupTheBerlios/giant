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
--  $RCSfile: giant-main.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/05/23 16:32:56 $
--
--
------------------------------------------------------------------------------
--
--  The GIANT main program.
--

with Gdk.Threads;
with Gtk.Main;

with Giant.Default_Logger;
with Giant.Main_Window;

procedure Giant.Main is
begin
   Giant.Default_Logger.Init;

   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gdk.Threads.Init;

   Gdk.Threads.Enter;
   Main_Window.Show;
   Gtk.Main.Main;
   Gdk.Threads.Leave;

   Giant.Default_Logger.Close;
end Giant.Main;
