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
--  $RCSfile: giant-clists.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/23 11:30:45 $
--
--  Provides an enhanced Gtk.Clist.
--
--  Pattern:
--    ADT
--

with Glib;
with Gtk.Clist;
with Gtk.Menu;

package Giant.Clists is

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with private;

   type Giant_Clist is access all Giant_Clist_Record'Class;

   procedure Create
     (List    :    out Giant_Clist;
      Columns : in     Glib.Gint);

   procedure Initialize
     (List    : access Giant_Clist_Record'Class;
      Columns : in     Glib.Gint);

   procedure Columns_Autosize
     (List  : access Giant_Clist_Record);

   procedure Connect_Popup_Menu
     (List : access Giant_Clist_Record;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Selected_Row
     (List : access Giant_Clist_Record)
     return Glib.Gint;

private

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with null
      record;

end Giant.Clists;
