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
-- $RCSfile: giant-gui_utils.adb,v $, $Revision: 1.2 $
-- $Author: squig $
-- $Date: 2003/05/23 19:03:25 $
--

with Gtk.Enums; use Gtk.Enums;
With Gtk.Frame;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Paned;
With Gtk.Scrolled_Window;
with Gtk.Widget;

package body Giant.Gui_Utils is

   function New_Button
     (Label    : in String;
      Callback : in Button_Callback.Marshallers.Void_Marshaller.handler)
     return Gtk.Button.Gtk_Button
   is
     Button : Gtk.Button.Gtk_Button;
   begin
      Gtk.Button.Gtk_New (button, Label);
      Button_Callback.Connect
        (Button, "clicked", Button_Callback.To_Marshaller (Callback));
      return Button;
   end New_Button;

   function New_Sub_Menu
     (Menu_Bar : in Gtk.Menu_Bar.Gtk_Menu_Bar;
      Label    : in String)
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

   function Add_Scrollbar_And_Frame
     (Widget : in Gtk.Widget.Gtk_Widget;
      Title  : in String)
     return Gtk.Frame.Gtk_Frame
   is
     Frame : Gtk.Frame.Gtk_Frame;
     scrolled_window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      --Set_Shadow_Type (Main_Window.Window_List_Frame, Shadow_Etched_In);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window,
                                      Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Widget);

      Gtk.Frame.Gtk_New (Frame, Title);
      Gtk.Frame.Add (Frame, Scrolled_window);

     return Frame;
   end Add_Scrollbar_And_Frame;

   function New_Column_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label
   is
      Label: Gtk.Label.Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, "Name");
      Gtk.Label.Set_Alignment (Label, 0.5, 0.5);
      Gtk.Label.Set_Padding (Label, 0, 0);
      Gtk.Label.Set_Justify (Label, Justify_Center);
      Gtk.Label.Set_Line_Wrap (Label, False);

      return Label;
   end;

end Giant.Gui_Utils;
