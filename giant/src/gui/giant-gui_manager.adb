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
--  $RCSfile: giant-gui_manager.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/17 16:58:34 $
--

with Gdk.Threads;
with Gtk.Main;

with Lists;

with Giant.Main_Window;
with Giant.Graph_Window;

package body Giant.Gui_Manager is

   Initialized : Boolean := False;

   package Graph_Window_Lists is new Lists (Graph_Window.Graph_Window_Access);
   Open_Windows : Graph_Window_Lists.List := Graph_Window_Lists.Create;

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

   procedure Add_Window
     (Name : in String)
   is
   begin
      Main_Window.Add_Window (Name);
   end Add_Window;

   procedure Close (Visual_Window : Vis_Windows.Visual_Window_Access)
   is
   begin
      null;
   end Close;

   procedure Open (Visual_Window : Vis_Windows.Visual_Window_Access)
   is
      Window : Graph_Window.Graph_Window_Access;
   begin
      Graph_Window.Create (Window, Visual_Window);
      Graph_Window_Lists.Attach (Open_Windows, Window);

      Graph_Window.Show_All (Window);
   end Open;

   procedure Remove_Window
     (Name : in String)
   is
   begin
      Main_Window.Remove_Window (Name);
   end Remove_Window;

end Giant.Gui_Manager;
