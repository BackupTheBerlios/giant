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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/05/31 19:23:40 $
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

   ---------------------------------------------------------------------------
   --
   --  Lazily creates Window_List_Menu.
   function Get_Window_List_Menu
     return Gtk.Menu.Gtk_Menu
   is
      use Gtk.Menu;
   begin
      if Window_List_Menu /= null then
         return Window_List_Menu;
      end if;

      Gtk.Menu.Gtk_New (Window_List_Menu);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Open...", On_Window_List_Open'Access));
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Close", On_Window_List_Close'Access));

      Gtk.Menu.Show_All (Window_List_Menu);

      return Window_List_Menu;
   end;


   function On_Window_List_Button_Press
     (Source : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event : Gdk.Event.Gdk_Event)
     return Boolean
   is
      use Glib;
      use Gdk.Types;

      Row : Gint;
      Column : Gint;
      Is_Valid : Boolean;
   begin
      if Gdk.Event.Get_Button (Event) = 3
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Types.Button_Press
      then
         Gtk.Clist.Get_Selection_Info (Window_List,
                                       Gint (Gdk.Event.Get_X (Event)),
                                       Gint (Gdk.Event.Get_Y (Event)),
                                       Row, Column, Is_Valid);
         if (Is_Valid) then
            Gtk.Clist.Select_Row (Window_List, Row, Column);
            Gtk.Menu.Popup (Get_Window_List_Menu,
                            Button => Gdk.Event.Get_Button (Event),
                            Activate_Time => Gdk.Event.Get_Time (Event));
            return True;
         end if;
      end if;
      return False;
   end On_Window_List_Button_Press;

   function Initialize_Menu
     return Gtk.Menu_Bar.Gtk_Menu_Bar is
      use Gtk.Menu_Bar;
      use Gtk.Menu;

      Menu_Bar : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
   begin
      Gtk_New (Menu_Bar);

      Menu := New_Sub_Menu (Menu_Bar, -"Projekt");
      Add (Menu, New_Menu_Item (-"New", On_Project_New'Access));
      Add (Menu, New_Menu_Separator);
      Add (Menu, New_Menu_Item (-"Quit", On_Project_Quit'Access));

      return Menu_Bar;
   end Initialize_Menu;

   function Initialize_Window_List
     return Gtk.Widget.Gtk_Widget
   is
      use Gtk.Clist;
      use Gtk.Widget;
   begin
      Gtk_New (Window_List, 2);
      Set_Show_Titles (Window_List, True);
      Clist_Return_Callback.Connect
        (Window_List, "button_press_event",
         Clist_Return_Callback.To_Marshaller
         (On_Window_List_Button_Press'Access));

      Set_Column_Title (Window_List, 0, -"Name");
      Set_Column_Title (Window_List, 1, -"Open");

      return Gtk_Widget
        (Add_Scrollbar_And_Frame (Gtk_Widget (Window_List), -"Windows"));
   end Initialize_Window_List;

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

      --  panels
      Pane := New_Vpaned;
      Gtk.Box.Add (Box, Pane);

      Gtk.Paned.Add (Pane, Initialize_Window_List);

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
