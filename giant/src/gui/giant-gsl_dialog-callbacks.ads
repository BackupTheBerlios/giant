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
--  $RCSfile: giant-gsl_dialog-callbacks.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/09/20 20:27:36 $
--
------------------------------------------------------------------------------
--
--  Provides callbacks for gsl dialogs.
--

with Gdk.Event;
with Gtk.Widget;

package Giant.Gsl_Dialog.Callbacks is

   ---------------------------------------------------------------------------
   --  File Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_File_Insert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_New
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Open
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Open_External
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Revert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Save
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Save_As
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Edit Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edit_Clear
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Copy
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Cut
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Paste
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Button Callbacks
   ---------------------------------------------------------------------------

   procedure On_Run_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Other Callbacks
   ---------------------------------------------------------------------------

   function On_Focus
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean;

end Giant.Gsl_Dialog.Callbacks;
