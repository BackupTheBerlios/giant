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
--  $RCSfile: giant-default_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/06/21 21:04:02 $
--
------------------------------------------------------------------------------
--
-- Provides a default dialog with a content area and a button panel.
--

with Gtk.Box;
with Gtk.Button;
with Gtk.Hbutton_Box;
with Gtk.Widget;
with Gtk.Window;

package Giant.Default_Dialog is

   type Input_Callback is access function (S: in String) return Boolean;

   type Response_Type is
      (Response_Okay, Response_Cancel, Response_Close, Response_Yes,
       Response_No);

   type Button_Type is
     (Button_None, Button_Cancel, Button_Close, Button_Okay_Cancel,
      Button_Yes_No, Button_Yes_No_Cancel);

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;

   type Default_Dialog_Access is access all Default_Dialog_Record'Class;

   procedure Add
     (Dialog : access Default_Dialog_Record'Class;
      Button : in     Gtk.Button.Gtk_Button);

   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Icon_Filename : in     String;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox;

   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Icon_Filename : in     String;
      Message       : in     String                      := "")
     return Gtk.Box.Gtk_Hbox;

   ---------------------------------------------------------------------------
   --  Called if one of the default buttons is pressed.
   --
   --  Sub-classes can overwrite this method.
   --
   --  Returns:
   --    True
   function Can_Hide
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog  :    out Default_Dialog_Access;
      Title   : in     String;
      Buttons : in     Button_Type);

   function Get_Center_Box
     (Dialog : access Default_Dialog_Record'class)
     return Gtk.Box.Gtk_Vbox;

   function Get_Response
     (Dialog : access Default_Dialog_Record'class)
     return Response_Type;

   procedure Initialize
     (Dialog  : access Default_Dialog_Record'class;
      Title   : in     String;
      Buttons : in     Button_Type);

   procedure Set_Center_Widget
     (Dialog : access Default_Dialog_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Button_Type := Button_Yes_No)
      return Response_Type;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error");

   function Show_Input_Dialog
     (Message       : in String;
      Title         : in String := -"Giant Input";
      Default_Input : in String := "")
     return String;

private

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with record
      Center_Box : Gtk.Box.Gtk_Vbox;
      Button_Box : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Response : Response_Type;
      Is_Modal : Boolean := False;
   end record;

end Giant.Default_Dialog;
