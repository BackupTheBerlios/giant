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
--  First Author: <unkown>
--
--  $RCSfile: giant-default_dialog.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/06/19 19:37:05 $
--

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded;

with Gtk.Box;
with Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Gentry;
with Gtk.Hbutton_Box;
with Gtk.Label;
with Gtk.Main;
with Gtk.Pixmap;
with Gtk.Separator;
with Gtk.Widget;
with Gtk.Window;

with Giant.Gui_Utils; use Giant.Gui_Utils;
--with Giant.Utils; use Giant.Utils;

package body Giant.Default_Dialog is

   procedure Hide
     (Source   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response : in     Response_Type)
   is
      Dialog : Default_Dialog_Access;
   begin
      Dialog := Default_Dialog_Access (Gtk.Widget.Get_Toplevel (Source));
      Dialog.Response := Response;

      if (Can_Hide (Default_Dialog_Access (Dialog))) then
         Hide (Dialog);
         if (Dialog.Is_Modal) then
            Gtk.Main.Main_Quit;
         end if;
      end if;
   end;

   procedure On_Cancel_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Cancel);
   end On_Cancel_Button_Clicked;

   procedure On_Close_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Close);
   end On_Close_Button_Clicked;

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean is
   begin
      Hide (Source, Response_Close);
      return True;
   end On_Delete;

   procedure On_No_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_No);
   end On_No_Button_Clicked;

   procedure On_Okay_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Okay);
   end On_Okay_Button_Clicked;

   procedure On_Yes_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Hide (Source, Response_Yes);
   end On_Yes_Button_Clicked;

   function Can_Hide
     (Dialog : access Default_Dialog_Record)
     return Boolean
   is
   begin
      return True;
   end Can_Hide;

   procedure Create
     (Dialog  :    out Default_Dialog_Access;
      Title   : in     String;
      Buttons : in     Button_Type)
   is
   begin
      Dialog := new Default_Dialog_Record;
      Initialize (Dialog, Title, Buttons);
   end Create;

   function Get_Center_Box
     (Dialog : access Default_Dialog_Record'class)
     return Gtk.Box.Gtk_Vbox
   is
   begin
      return Dialog.Center_Box;
   end Get_Center_Box;

   function Get_Response
     (Dialog : access Default_Dialog_Record'class)
     return Response_Type
   is
   begin
      return Dialog.Response;
   end Get_Response;

   procedure Initialize
     (Dialog  : access Default_Dialog_Record'class;
      Title   : in     String;
      Buttons : in     Button_Type)
   is
      Button : Gtk.Button.Gtk_Button;
      Result : Boolean;
   begin
      Gtk.Window.Initialize (Dialog, Window_Toplevel);
      Set_Modal (Dialog, Dialog.Is_Modal);
      Set_Title (Dialog, Title);

      --  center box
      Gtk.Box.Gtk_New_Vbox (Dialog.Center_Box);
      Gtk.Box.Set_Border_Width (Dialog.Center_Box, DEFAULT_SPACING);
      Add (Dialog, Dialog.Center_Box);

      --  button box
      Gtk.Hbutton_Box.Gtk_New (Dialog.Button_Box);
      Gtk.Hbutton_Box.Set_Spacing (Dialog.Button_Box, BUTTON_SPACING);
      Gtk.Hbutton_Box.Set_Layout (Dialog.Button_Box, Buttonbox_Spread);
      Gtk.Box.Pack_End (Dialog.Center_Box, Dialog.Button_Box,
                        Expand => False, Fill => True,
                        Padding => DEFAULT_SPACING);

      --  buttons
      if (Buttons = Button_Close) then
         Button := New_Button (-"Close", On_Close_Button_Clicked'access);
         Gtk.Hbutton_Box.Add (Dialog.Button_Box, Button);
         Gtk.Button.Grab_Default (Button);
      elsif (Buttons = Button_Okay_Cancel) then
         Button := New_Button (-"Okay", On_Okay_Button_Clicked'access);
         Gtk.Hbutton_Box.Add (Dialog.Button_Box, Button);
         Gtk.Button.Grab_Default (Button);
      elsif (Buttons = Button_Yes_No
             or else Buttons = Button_Yes_No_Cancel) then
         Button := New_Button (-"Yes", On_Yes_Button_Clicked'access);
         Gtk.Hbutton_Box.Add (Dialog.Button_Box, Button);
         Gtk.Button.Grab_Default (Button);
         Gtk.Hbutton_Box.Add (Dialog.Button_Box,
                              New_Button (-"No", On_No_Button_Clicked'access));
      end if;

      if (Buttons = Button_Cancel
          or else Buttons = Button_Okay_Cancel
          or else Buttons = Button_Yes_No_Cancel) then
         Gtk.Hbutton_Box.Add (Dialog.Button_Box,
                              New_Button (-"Cancel",
                                          On_Cancel_Button_Clicked'access));
      end if;

      --  horizontal separator
      Gtk.Box.Pack_End (Dialog.Center_Box, New_Hseperator, Expand => False,
                        Fill => True, Padding => DEFAULT_SPACING);

      --  set position
      Set_Position (Dialog, Win_Pos_Mouse);

      --  connect close button
      Widget_Return_Callback.Connect
        (Dialog, "delete_event",
         Widget_Return_Callback.To_Marshaller (On_Delete'Access));
   end Initialize;

   procedure Add
     (Dialog : access Default_Dialog_Record'Class;
      Button : in Gtk.Button.Gtk_Button)
   is
   begin
      Gtk.Hbutton_Box.Add (Dialog.Button_Box, Button);
   end;

   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record'Class;
      Icon_Filename : in String;
      Message       : in String         := "")
     return Gtk.Box.Gtk_Hbox
   is
      Box : Gtk.Box.Gtk_Hbox;
      Label : Gtk.Label.Gtk_Label;
      Pixmap : Gtk.Pixmap.Gtk_Pixmap;
   begin
      Gtk.Box.Gtk_New_Hbox (Box);
      Set_Center_Widget (Dialog, Box);

      Pixmap := Gtk.Pixmap.Create_Pixmap (Icon_Filename, Dialog);
      --Gtk.Pixmap.Set_Alignment (Dialog.Confirmation_Msg_Pixmap, 0.5, 0.5);
      Gtk.Box.Pack_Start (Box, pixmap, expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

      Gtk.Label.Gtk_New (Label, Message);
      --Set_Alignment (Dialog.Confirmation_Message, 0.5, 0.5);
      Gtk.Label.Set_Justify (Label, Justify_Center);
      Gtk.Label.Set_Line_Wrap (Label, False);
      Gtk.Box.Pack_Start (Box, Label, Expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);

      return Box;
   end Add_Icon_Box;

   procedure Set_Center_Widget
     (Dialog : access Default_Dialog_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Box.Pack_Start (Dialog.Center_Box, Widget,
                          Expand => True, Fill => True,
                          Padding => DEFAULT_SPACING);
      --Gtk.Box.Reorder_Child (Dialog.Center_Box, Widget, 0);
   end Set_Center_Widget;

   procedure Show_Modal
     (Dialog : access Default_Dialog_Record'Class)
   is
   begin
      Dialog.Is_Modal := True;

      Show_All (Dialog);
      Gtk.Main.Main;
   end Show_Modal;

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Button_Type := Button_Yes_No)
     return Response_Type
   is
      Dialog : Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Create (Dialog, -"Giant Question", Buttons);
      Box := Add_Icon_Box (Dialog, "gnome-question.xpm", Message);

      Show_Modal (Dialog);
      Destroy (Dialog);

      return Dialog.Response;
   end Show_Confirmation_Dialog;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error")
   is
      Dialog : Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Create (Dialog, Title, Button_Close);
      Box := Add_Icon_Box (Dialog, "gnome-error.xpm", Message);

      Show_Modal (Dialog);
      Destroy (Dialog);
   end Show_Error_Dialog;


   function Show_Input_Dialog
     (Message       : in String;
      Title         : in String := -"Giant Input";
      Default_Input : in String := "")
      return String
   is
      Dialog : Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
      Input : Gtk.Gentry.Gtk_Entry;
   begin
      Create (Dialog, Title, Button_Okay_Cancel);
      Box := Add_Icon_Box (Dialog, "gnome-question.xpm", Message);
      Gtk.Gentry.Gtk_New (Input);
      Gtk.Gentry.Set_Text (Input, Default_Input);
      Gtk.Box.Add (Box, Input);

      Gtk.Gentry.Set_Flags (Input, Gtk.Widget.Can_Default);
      Gtk.Gentry.Grab_Default (Input);

      Show_Modal (Dialog);

      if (Dialog.Response = Response_Okay) then
         declare
            S : constant String := Gtk.Gentry.Get_Text (Input);
         begin
            Destroy (Dialog);
            return S;
         end;
      else
         Destroy (Dialog);
         return "";
      end if;
   end Show_Input_Dialog;

end Giant.Default_Dialog;
