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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/06/03 19:20:59 $
--

with Gdk.Event;
with Gdk.Types;
with Glib;
with Gtk.Box;
with Gtk.Clist;
pragma Elaborate_All (Gtk.Clist);
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Paned;
with Gtk.Tearoff_Menu_Item;
with Gtkada.Types;
with Interfaces.C.Strings;

with Giant.Gui_Utils; use Giant.Gui_Utils;
--  with Vis_Window_Management;

package body Giant.Main_Window is

   package Window_List_Data is new
     Gtk.Clist.Row_Data (String);

   --  main window instance
   Window : Gtk.Window.Gtk_Window;
   Window_List : Gtk.Clist.Gtk_Clist;
   Window_List_Menu : Gtk.Menu.Gtk_Menu;

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean is
   begin
      Quit;
      return True;
   end On_Delete;

   procedure On_Project_Quit
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Quit;
   end On_Project_Quit;

   procedure On_Project_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Project_New;

   procedure On_Window_List_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Window_List_Open;

   procedure On_Window_List_Close
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Window_List_Close;

   function Initialize_Menu
     return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);

      Menu := New_Sub_Menu (Menu_Bar, -"Projekt");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Project_New'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Quit", On_Project_Quit'Access));

      return Menu_Bar;
   end Initialize_Menu;

   procedure Initialize is
      Box : Gtk.Box.Gtk_Vbox;
      Pane : Gtk.Paned.Gtk_Vpaned;
   begin
      Gtk.Window.Initialize (Window, Window_Toplevel);
      Gtk.Window.Set_Title (Window, -"GIANT");

      Gtk.Box.Gtk_New_Vbox (Box);
      Gtk.Window.Add (Window, Box);

      --  menu
      Gtk.Box.Pack_Start (Box, Initialize_Menu, False, False, 0);

      --  split pane
      Pane := New_Vpaned;
      Gtk.Box.Add (Box, Pane);

      --  window list popup menu
      Gtk.Menu.Gtk_New (Window_List_Menu);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Open...", On_Window_List_Open'Access));
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Close", On_Window_List_Close'Access));

      --  window list
      Gtk.Clist.Gtk_New (Window_List, 2);
      Gtk.Clist.Set_Show_Titles (Window_List, True);
      Connect_Popup_Menu (Window_List, Window_List_Menu);

      Gtk.Clist.Set_Column_Title (Window_List, 0, -"Name");
      Gtk.Clist.Set_Column_Title (Window_List, 1, -"Open");

      Gtk.Paned.Add (Pane, Add_Scrollbar_And_Frame (Window_List, -"Windows"));

      --  connect close button
      Widget_Return_Callback.Connect
        (Window, "delete_event",
         Widget_Return_Callback.To_Marshaller (On_Delete'Access));
   end Initialize;

   procedure Quit
   is
   begin
      -- FIX: ask user to save project
      Gtk.Main.Main_Quit;
   end Quit;

   procedure Show
   is
      use Gtkada.Types;

      Row_Data : Gtkada.Types.Chars_Ptr_Array (0 .. 1);
      Row : Glib.Gint;
   begin
      Gtk.Window.Gtk_New (Window);

      Initialize;

      for I in 1 .. 8 loop
         Row_Data(0) := Interfaces.C.Strings.New_String ("Call_Graph"
                                                         & Integer'Image (I));
         Row_Data(1) := Interfaces.C.Strings.New_String ("X");
         Row := Gtk.Clist.Append (Window_List, Row_Data);
         Window_List_Data.Set (Window_List, Row, "custom_data");
         Free (Row_Data);
      end loop;

      Gtk.Window.Show_All (Window);
   end Show;

end Giant.Main_Window;
