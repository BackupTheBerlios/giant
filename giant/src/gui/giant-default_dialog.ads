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
--  $RCSfile: giant-default_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/05/23 19:03:24 $
--
------------------------------------------------------------------------------
--
-- Provides a default dialog with a content area and a button panel.
--

with Gtk.Box;
with Gtk.Hbutton_Box;
with Gtk.Window;

package Giant.Default_Dialog is
   
   type Response_Type is
	 (Response_Okay, Response_Cancel, Response_Yes, Response_No);
	 
   type Button_Type is
	 (Button_None, Button_Close, Button_Okay_Cancel, Button_Yes_No);
   
   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with record
      Center_Box : Gtk.Box.Gtk_Vbox;
	  Button_Box : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
	  Response : Response_Type;
   end record;
   
   type Default_Dialog is
	 access all Default_Dialog_Record'Class;
   
   procedure Create 
	 (Dialog  :    out Default_Dialog;
	  Title	  : in     String;
	  Buttons : in     Button_Type);
	 
   function Add_Icon_Box 
	 (Dialog		: in Default_Dialog;
	  Icon_Filename	: in String;
	  Message		: in String			:= "")
	 return Gtk.Box.Gtk_Hbox;
   
   procedure Show_Error (Message : in String);

end Giant.Default_Dialog;
