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
--  $RCSfile: giant-gui_manager.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/06/16 15:27:58 $
--

with Gdk.Threads;
with Gtk.Main;

with Lists;

with Giant.Main_Window;
with Giant.Graph_Window;

package body Giant.Gui_Manager is

   Initialized : Boolean := False;

   type Window_Record is record
      Visual_Window : Vis_Windows.Visual_Window_Access;
      Graph : Graph_Window.Graph_Window_Access;
   end record;

   package Window_Lists is new Lists (Window_Record);
   Window_List : Window_Lists.List := Window_Lists.Create;

   procedure Quit
   is
   begin
      Main_Window.Quit;
   end Quit;

   procedure Show
   is
   begin
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;
      Gdk.Threads.Init;

      Gdk.Threads.Enter;
      Main_Window.Show;
      Gtk.Main.Main;
      Gdk.Threads.Leave;
   end Show;

   procedure Add (Visual_Window : Vis_Windows.Visual_Window_Access)
   is
      Window : Window_Record;
   begin
      Window.Visual_Window := Visual_Window;
      Graph_Window.Create (Window.Graph);

      Window_Lists.Attach (Window_List, Window);
   end Add;

end Giant.Gui_Manager;
