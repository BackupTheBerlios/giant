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
-- $RCSfile: giant-gui_utils.adb,v $, $Revision: 1.7 $
-- $Author: squig $
-- $Date: 2003/06/17 20:28:40 $
--

with Glib;
with Gdk.Event;
with Gdk.Types;
with Gtk.Enums; use Gtk.Enums;
With Gtk.Frame;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Paned;
With Gtk.Scrolled_Window;
with Gtk.Tearoff_Menu_Item;
with Gtk.Widget;
with Gtk.Window;

package body Giant.Gui_Utils is

   package Clist_User_Return_Callback is new
     Gtk.Handlers.User_Return_Callback (Gtk.Clist.Gtk_Clist_Record, Boolean,
                                        Gtk.Menu.Gtk_Menu);

   function On_Clist_Button_Press
     (Source : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event;
      Menu   : in     Gtk.Menu.Gtk_Menu)
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
         Gtk.Clist.Get_Selection_Info (Source,
                                       Gint (Gdk.Event.Get_X (Event)),
                                       Gint (Gdk.Event.Get_Y (Event)),
                                       Row, Column, Is_Valid);
         if (Is_Valid) then
            Gtk.Clist.Select_Row (Source, Row, Column);
            Gtk.Menu.Show_All (Menu);
            Gtk.Menu.Popup (Menu,
                            Button => Gdk.Event.Get_Button (Event),
                            Activate_Time => Gdk.Event.Get_Time (Event));
            return True;
         end if;
      end if;
      return False;
   end On_Clist_Button_Press;

   procedure Connect_Popup_Menu
     (List : access Gtk.Clist.Gtk_Clist_Record'Class;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      Clist_User_Return_Callback.Connect
        (List, "button_press_event",
         Clist_User_Return_Callback.To_Marshaller
         (On_Clist_Button_Press'Access), Gtk.Menu.Gtk_Menu (Menu));
   end Connect_Popup_Menu;

   function Get_Selected_Row
     (List : access Gtk.Clist.Gtk_Clist_Record'Class)
     return Glib.Gint
   is
      use type Gint_List.Glist;
      Selection : constant Gint_List.Glist := Gtk.Clist.Get_Selection (List);
   begin
      if Selection /= Gint_List.Null_List then
         return Gint_List.Get_Data (Gint_List.First (Selection));
      end if;

      return Glib.Gint (-1);
   end Get_Selected_Row;

   function New_Button
     (Label    : in String;
      Callback : in Button_Callback.Marshallers.Void_Marshaller.handler)
     return Gtk.Button.Gtk_Button
   is
     Button : Gtk.Button.Gtk_Button;
   begin
      Gtk.Button.Gtk_New (Button, Label);
      Gtk.Button.Set_Flags (Button, Gtk.Widget.Can_Default);
      Button_Callback.Connect
        (Button, "clicked", Button_Callback.To_Marshaller (Callback));
      return Button;
   end New_Button;

   function New_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label
   is
      Label: Gtk.Label.Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, Title);
      Gtk.Label.Set_Line_Wrap (Label, False);
      return Label;
   end;

   function New_Sub_Menu
     (Menu_Bar : access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class;
      Label    : in     String)
     return Gtk.Menu.Gtk_Menu
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Menu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu_item.Gtk_New (Item, label);
      Gtk.Menu_Bar.Add (Menu_Bar, Item);

      Gtk.Menu.Gtk_New (Menu);
      Gtk.Menu_Item.Set_Submenu (Item, Menu);

      return Menu;
   end;

   function New_Menu_Item
     (Label    : in String;
      Callback : in Menu_Item_Callback.Marshallers.Void_Marshaller.Handler)
      return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Item.Gtk_New (Item, Label);
      Menu_Item_Callback.Connect
        (Item, "activate", Menu_Item_Callback.To_Marshaller (Callback));
      return Item;
   end New_Menu_Item;

   function New_Menu_Separator
      return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Item.Gtk_New (item);
      return Item;
   end New_Menu_Separator;

   function New_Vpaned
     return Gtk.Paned.Gtk_Vpaned
   is
      Pane : Gtk.Paned.Gtk_Vpaned;
   begin
      Gtk.Paned.Gtk_New_Vpaned (pane);
      Gtk.Paned.Set_Border_Width (pane, DEFAULT_SPACING);
      Gtk.Paned.Set_Handle_Size (Pane, 8);
      Gtk.Paned.Set_Gutter_Size (Pane, 12);
      return Pane;
   end New_Vpaned;

   function Add_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'class;
      Title  : in     String)
     return Gtk.Frame.Gtk_Frame
   is
     Frame : Gtk.Frame.Gtk_Frame;
   begin
      Gtk.Frame.Gtk_New (Frame, Title);
      Gtk.Frame.Set_Shadow_Type (Frame, Shadow_Etched_In);
      Gtk.Frame.Add (Frame, Widget);
      return Frame;
   end Add_Frame;

   function Add_Scrollbar_And_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Title  : in     String)
     return Gtk.Frame.Gtk_Frame
   is
     Frame : Gtk.Frame.Gtk_Frame;
     Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Widget);

      Gtk.Frame.Gtk_New (Frame, Title);
      Gtk.Frame.Set_Shadow_Type (Frame, Shadow_Etched_In);
      Gtk.Frame.Add (Frame, Scrolled_window);

      return Frame;
   end Add_Scrollbar_And_Frame;

   function New_Column_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label
   is
      Label: Gtk.Label.Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, Title);
      Gtk.Label.Set_Alignment (Label, 0.5, 0.5);
      Gtk.Label.Set_Padding (Label, 0, 0);
      Gtk.Label.Set_Justify (Label, Justify_Center);
      Gtk.Label.Set_Line_Wrap (Label, False);
      return Label;
   end;

   function New_TearOff_Menu_Item
     return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Item : Gtk.TearOff_Menu_Item.Gtk_Tearoff_Menu_Item;
   begin
      Gtk.TearOff_Menu_Item.Gtk_New (item);
      return Gtk.Menu_Item.Gtk_Menu_Item (Item);
   end New_TearOff_Menu_Item;


   procedure Set_Default
     (Window : access Gtk.Window.Gtk_Window_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Result : Boolean;
   begin
      Gtk.Widget.Set_Flags (Widget, Gtk.Widget.Can_Default);
      Gtk.Widget.Grab_Default (Widget);
      Result := Gtk.Window.Activate_Default (Window);
   end;

   package body Clist_Row_Data is

      function Find
        (List  : access Gtk.Clist.Gtk_Clist_Record'Class;
         Value : in     Data_Type)
         return Glib.Gint
      is
         use type Glib.Gint;
      begin
         for I in 0 .. Gtk.Clist.Get_Rows (List) - 1 loop
            if (Data.Get (List, I) = Value) then
               return I;
            end if;
         end loop;

         return Glib.Gint (-1);
      end Find;

   end Clist_Row_Data;

end Giant.Gui_Utils;
