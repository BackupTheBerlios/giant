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
--  $RCSfile: giant-gui_utils.ads,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/06/19 16:38:06 $
--
------------------------------------------------------------------------------
--
--  Provides common gui utility methods.
--

with Glib;
with Gtk.Button;
with Gtk.Clist;
with Gtk.Editable;
With Gtk.Frame;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Widget;
with Gtk.Window;

with Giant.Clists;

package Giant.Gui_Utils is

   pragma Elaborate_Body;

   -----------------
   --  Constants  --
   -----------------

   DEFAULT_SPACING : constant Glib.Gint := 5;
   BUTTON_SPACING : constant Glib.Gint := 10;

   -----------------
   --  Callbacks  --
   -----------------

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk.Button.Gtk_Button_Record);

   package Clist_Callback is new
     Gtk.Handlers.Callback (Gtk.Clist.Gtk_Clist_Record);

   package Clist_Return_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Clist.Gtk_Clist_Record, Boolean);

   package Editable_Callback is new
     Gtk.Handlers.Callback (Gtk.Editable.Gtk_Editable_Record);

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk.Menu_Item.Gtk_Menu_Item_Record);

   package Widget_Return_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Widget.Gtk_Widget_Record, Boolean);

   ---------------------------------------------------------------------------
   --  Generic Packages
   ---------------------------------------------------------------------------

   package String_Clists is new
     Giant.Clists (String);

   ---------------
   --  Methods  --
   ---------------

   function Add_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'class;
      Title  : in     String)
     return Gtk.Frame.Gtk_Frame;

   function Add_Scrollbar_And_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'class;
      Title  : in String)
     return Gtk.Frame.Gtk_Frame;

   procedure Connect_Popup_Menu
     (List : access Gtk.Clist.Gtk_Clist_Record'Class;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Selected_Row
     (List : access Gtk.Clist.Gtk_Clist_Record'Class)
     return Glib.Gint;

   function New_Button
     (Label    : in String;
      Callback : in Button_Callback.Marshallers.Void_Marshaller.handler)
     return Gtk.Button.Gtk_Button;

   function New_Column_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label;

   function New_Hseperator
     return Gtk.Separator.Gtk_Hseparator;

   function New_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label;

   function New_Menu_Item
     (Label    : in String;
      Callback : in Menu_Item_Callback.Marshallers.Void_Marshaller.Handler)
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Menu_Separator
      return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Sub_Menu
     (Menu_Bar : access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class;
      Label    : in String)
     return Gtk.Menu.Gtk_Menu;

   function New_TearOff_Menu_Item
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Vpaned
     return Gtk.Paned.Gtk_Vpaned;

   procedure Set_Default
     (Window : access Gtk.Window.Gtk_Window_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

end Giant.Gui_Utils;


