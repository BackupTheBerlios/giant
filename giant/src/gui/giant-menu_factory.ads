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
--  $RCSfile: giant-menu_factory.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/08/15 16:37:18 $
--
--  Creates menu items for GSL script entries.
--

with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Widget;

package Giant.Menu_Factory is

   type Script_Event (Label_Length : Natural) is record
     Widget : Gtk.Widget.Gtk_Widget;
     Label  : String (1 .. Label_Length);
   end record;

   type Script_Event_Access is access Script_Event;

   package Widget_User_Callback is new
     Gtk.Handlers.User_Callback (Gtk.Widget.Gtk_Widget_Record,
                                 Script_Event);


   type Script_Callback_Type is access procedure
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Data   : in     Script_Event);

   procedure Generate
     (Labels    : in     String;
      Separator : in     String;
      Menu      : in     Gtk.Menu.Gtk_Menu;
      Callback  : in     Widget_User_Callback.Marshallers.Void_Marshaller.Handler;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

end Giant.Menu_Factory;
