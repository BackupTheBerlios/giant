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
-- $RCSfile: giant-gui_utils.adb,v $, $Revision: 1.19 $
-- $Author: squig $
-- $Date: 2003/07/10 21:54:53 $
--

with Glib;
with Gdk.Event;
with Gdk.Types;
with Gtk.Arguments;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tearoff_Menu_Item;

with Gnat.OS_Lib;

with Giant.Config;
with Giant.Config.Global_Data;
with Giant.Dialogs;
with Giant.Logger;

package body Giant.Gui_Utils is

   package Logger is new Giant.Logger("giant.gui_utils");

   procedure Add_Row_Widgets
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in out Glib.Guint;
      Left : access Gtk.Widget.Gtk_Widget_Record'Class;
      Right : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Glib.Guint;
   begin
      Gtk.Table.Attach (Table, Left,
                        Left_Attach => 0, Right_Attach => 1,
                        Top_Attach => Row, Bottom_Attach => Row + 1,
                        Xoptions => Gtk.Enums.Fill);
      Gtk.Table.Attach (Table, Right,
                        Left_Attach => 1, Right_Attach => 2,
                        Top_Attach => Row, Bottom_Attach => Row + 1);
      Row := Row + 1;
   end Add_Row_Widgets;

   procedure Add_Row
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in out Glib.Guint;
      Left  : access Gtk.Misc.Gtk_Misc_Record'Class;
      Right : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Glib.Guint;
   begin
      --  right align
      Gtk.Misc.Set_Alignment (Left, 1.0, 0.5);
      Add_Row_Widgets (Table, Row, Left, Right);
   end;

   procedure Add_Row_Labels
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in out Glib.Guint;
      Left  : access Gtk.Misc.Gtk_Misc_Record'Class;
      Right : access Gtk.Misc.Gtk_Misc_Record'Class)
   is
      use type Glib.Guint;
   begin
      --  left align
      Gtk.Misc.Set_Alignment (Left, 0.0, 0.5);
      Gtk.Misc.Set_Alignment (Right, 0.0, 0.5);
      Add_Row_Widgets (Table, Row, Left, Right);
   end Add_Row_Labels;

   function Get_Icon
     (Filename : in String)
     return String
   is
   begin
      Logger.Debug ("Loading icon: "
                    & Config.Global_Data.Get_Resources_Directory & "icons"
                    & GNAT.OS_Lib.Directory_Separator & Filename);
      return Config.Global_Data.Get_Resources_Directory & "icons"
        & GNAT.OS_Lib.Directory_Separator & Filename;
   end;

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

   procedure Handle_IO_Exception
     (Error    : in Ada.Exceptions.Exception_Occurrence;
      Filename : in String)
   is
   begin
      Ada.Exceptions.Reraise_Occurrence (Error);
   exception
     when Ada.IO_Exceptions.Data_Error =>
        Dialogs.Show_Error_Dialog (-"Unexpected data type: " & Filename);
     when Ada.IO_Exceptions.Device_Error =>
        Dialogs.Show_Error_Dialog (-"Device error during io operation: " & Filename);
     when Ada.IO_Exceptions.End_Error =>
        Dialogs.Show_Error_Dialog (-"Unexpected end of file: " & Filename);
     when Ada.IO_Exceptions.Layout_Error =>
        Dialogs.Show_Error_Dialog (-"Unexpected file layout: " & Filename);
     when Ada.Io_Exceptions.Name_Error =>
        Dialogs.Show_Error_Dialog(-"File not found: " & Filename);
     when Ada.IO_Exceptions.Status_Error =>
        Dialogs.Show_Error_Dialog (-"File already open: " & Filename);
   end Handle_IO_Exception;

   function New_Button
     (Label    : in String;
      Callback : in Button_Callback.Marshallers.Void_Marshaller.Handler)
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

   function New_Button
     (Label    : in     String;
      Callback : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Button.Gtk_Button
   is
     Button : Gtk.Button.Gtk_Button;
   begin
      Gtk.Button.Gtk_New (Button, Label);
      Gtk.Button.Set_Flags (Button, Gtk.Widget.Can_Default);
      Widget_Callback.Object_Connect
        (Button, "clicked", Widget_Callback.To_Marshaller (Callback), Widget);
      return Button;
   end New_Button;

   function New_Hseperator
     return Gtk.Separator.Gtk_Hseparator
   is
      Separator : Gtk.Separator.Gtk_HSeparator;
   begin
      Gtk.Separator.Gtk_New_Hseparator (Separator);
      return Separator;
   end New_Hseperator;

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
     (Menu_Bar : access Gtk.Menu_Shell.Gtk_Menu_Shell_Record'Class;
      Label    : in     String)
     return Gtk.Menu.Gtk_Menu
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Menu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu_Item.Gtk_New (Item, label);
      Gtk.Menu_Shell.Append (Menu_Bar, Item);

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

   function New_Menu_Item
     (Label    : in     String;
      Callback : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Item.Gtk_New (Item, Label);
      Widget_Callback.Object_Connect
        (Item, "activate", Widget_Callback.To_Marshaller (Callback),
         Widget);
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

   function Add_Scrollbars
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Scrolled_Window.Gtk_Scrolled_Window
   is
     Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Automatic);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Widget);
      return Scrolled_Window;
   end Add_Scrollbars;

   function Add_Scrollbars_And_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Title  : in     String)
     return Gtk.Frame.Gtk_Frame
   is
   begin
      return Add_Frame (Add_Scrollbars (Widget), Title);
   end Add_Scrollbars_And_Frame;

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

end Giant.Gui_Utils;
