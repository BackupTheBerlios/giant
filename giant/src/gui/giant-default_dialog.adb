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
--  $RCSfile: giant-default_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/05/23 19:03:24 $
--

with Gtk.Box;
with Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
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
   
   procedure On_Cancel_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
	  Parent : Default_Dialog;
   begin
	  Parent := Default_Dialog (Gtk.Widget.Get_Toplevel
								(Gtk.Widget.Gtk_Widget (Source)));
	  Parent.Response := Response_Cancel;
      Gtk.Main.Main_Quit;
   end On_Cancel_Button_Clicked;

   procedure On_Close_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Close_Button_Clicked;
   
   procedure On_No_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_No_Button_Clicked;
   
   procedure On_Okay_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Okay_Button_Clicked;
   
   procedure On_Yes_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
	  Parent : Default_Dialog;
   begin
	  Parent := Default_Dialog (Gtk.Widget.Get_Toplevel
								(Gtk.Widget.Gtk_Widget (Source)));
	  Parent.Response := Response_Yes;
      Gtk.Main.Main_Quit;
   end On_Yes_Button_Clicked;

   procedure Create 
	 (Dialog  :    out Default_Dialog;
	  Title	  : in     String;
	  Buttons : in     Button_Type)
   is
	  Separator : Gtk.Separator.Gtk_Separator;
   begin
	  Dialog := new Default_Dialog_Record;
	  
	  Gtk.Window.Initialize (Dialog, Window_Toplevel);
	  Set_Title (Dialog, Title);

	  Gtk.Box.Gtk_New_Vbox (Dialog.Center_Box);
	  Gtk.Box.Set_Border_Width (Dialog.Center_Box, DEFAULT_SPACING);
	  Add (Dialog, Dialog.Center_Box);
	  
	  -- separator
	  Gtk.Separator.Gtk_New_Hseparator (Separator);
	  Gtk.Box.Pack_Start (Dialog.Center_Box, Separator, Expand => False, 
						  Fill => True, Padding => DEFAULT_SPACING);
	  
	  -- button box
	  Gtk.Hbutton_Box.Gtk_New (Dialog.Button_Box);
	  Gtk.Hbutton_Box.Set_Spacing (Dialog.Button_Box, BUTTON_SPACING);
	  Gtk.Hbutton_Box.Set_Layout (Dialog.Button_Box, Buttonbox_Spread);
	  Gtk.Box.Add (Dialog.Center_Box, Dialog.Button_Box);
	  
	  if (Buttons = Button_Close) then
		 Gtk.Hbutton_Box.Add (Dialog.Button_Box, 
							  New_Button (-"Close", 
										  On_Close_Button_Clicked'access));	 
	  elsif (Buttons = Button_Yes_No) then
		 Gtk.Hbutton_Box.Add (Dialog.Button_Box, 
							  New_Button (-"Yes", On_Yes_Button_Clicked'access));
		 Gtk.Hbutton_Box.Add (Dialog.Button_Box, 
							  New_Button (-"No", On_Yes_Button_Clicked'access));
	  end if;
   end Create;
	  
   function Add_Icon_Box 
	 (Dialog		: in Default_Dialog;
	  Icon_Filename	: in String;
	  Message		: in String			:= "")
	 return Gtk.Box.Gtk_Hbox
   is 
	  Box : Gtk.Box.Gtk_Hbox;
	  Label : Gtk.Label.Gtk_Label;
	  Pixmap : Gtk.Pixmap.Gtk_Pixmap;
   begin
	  Gtk.Box.Gtk_New_Hbox (Box);
	  Gtk.Box.Pack_Start (Dialog.Center_Box, Box, Expand => True, 
						  Fill => True, Padding => DEFAULT_SPACING);
	  Gtk.Box.Reorder_Child (Dialog.Center_Box, Box, 0);
	  
	  Pixmap := Gtk.Pixmap.Create_Pixmap (Icon_Filename, Dialog);
	  --Gtk.Pixmap.Set_Alignment (Dialog.Confirmation_Msg_Pixmap, 0.5, 0.5);
	  Gtk.Box.Pack_Start (Box, pixmap, expand => True, Fill => True, 
						  Padding => DEFAULT_SPACING);
	  
	  Gtk.Label.Gtk_New (Label, Message);
	  --Set_Alignment (Dialog.Confirmation_Message, 0.5, 0.5);
	  Gtk.Label.Set_Justify (Label, Justify_Center);
	  Gtk.Label.Set_Line_Wrap (Label, False);
	  Gtk.Box.Add (Box, Label);
	  
	  return Box;
   end Add_Icon_Box;
   
   procedure Show_Modal
	 (Dialog : in Default_Dialog)
   is
   begin
      Show_All (Dialog);
      Gtk.Main.Main;
      Destroy (Dialog);
   end Show_Modal;
   
   function Show_Confirmation
	 (Message : in String) 
	 return Response_Type
   is
      Dialog : Default_Dialog;
	  Box : Gtk.Box.Gtk_Hbox;
   begin
	  Create (Dialog, -"Giant Question", Button_Yes_No);
      Box := Add_Icon_Box (Dialog, "gnome-question.xpm", Message);
	  
	  Show_Modal (Dialog);
	  
	  return Dialog.Response;
   end Show_Confirmation;

   procedure Show_Error
	 (Message : in String)
   is
      Dialog : Default_Dialog;
	  Box : Gtk.Box.Gtk_Hbox;
   begin
	  Create (Dialog, -"Giant Error", Button_Close);
      Box := Add_Icon_Box (Dialog, "gnome-error.xpm", Message);
	  
	  Show_Modal (Dialog);
   end Show_Error;
   
--     procedure Show_Input
--  	 (Message : in     String;
--  	  Boolean :    out Cancelled)
--  	 return String
--     is
--        Dialog : Default_Dialog;
--  	  Box : Gtk.Box.Gtk_Hbox;
--  	  Input : Gtk.Gentry.Gtk_Gentry;
--     begin
--  	  Create (Dialog, -"Giant Input", Button_Yes_No);
--        Box := Add_Icon_Box (Dialog, "gnome-question.xpm", Message);
--  	  Gtk.Gentry.Gtk_Entry.Gtk_New (Input, 20);
--  	  Gtk.Box.Add (Box, Input);
	  
--  	  Show_Modal (Dialog);
--  	  Cancelled := (Dialog.Response = Response_Okay);
	  
--  	  return Gtk.Gentry.Get_Text (Input);
--     end Show_Input;

end Giant.Default_Dialog;
