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
--  $RCSfile: giant-gsl_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/05/31 19:23:40 $
--

with Ada.Text_Io; use Ada.Text_Io;

with Gtkada.File_Selection;
with Gtk.Box;
with Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Scrolled_Window;
with Gtk.Text;
with Gtk.Widget;

with Giant.Default_Dialog; 
with Giant.Gui_Utils; use Giant.Gui_Utils;
--with Giant.Utils; use Giant.Utils;

package body Giant.Gsl_Dialog is

   procedure Read 
	 (Dialog   : Gsl_Dialog_Access;
	  Filename : String)
   is
	  In_File : Ada.Text_Io.File_Type;
	  Line : String(1..255);
	  Last : Integer;
   begin
	  Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File, Filename);

	  while (not Ada.Text_Io.End_Of_File (In_File)) loop
		 Ada.Text_Io.Get_Line (In_File, Line, Last);
		 Put_Line (Line, Last);
	  end loop;

	  Ada.Text_IO.Close (In_File);
--     exception
--  	  when others =>
--  		 Default_Dialog.Show_Error(-"Could not read file: " & Filename);
   end;

   procedure On_Open_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
	  Dialog : Gsl_Dialog_Access;
   begin
      Dialog := Gsl_Dialog_Access (Gtk.Widget.Get_Toplevel
								   (Gtk.Widget.Gtk_Widget (Source)));
	  declare
		 Filename : String := Gtkada.File_Selection.File_Selection_Dialog
		   (-"Open GSL Script",  
			Ada.Strings.Unbounded.To_String (Dialog.Filename), 
			Dir_Only => False, Must_Exist => True);
	  begin
		 if (Filename /= "") then
			Dialog.Filename
			  := Ada.Strings.Unbounded.To_Unbounded_String (Filename);
			Read (Dialog, Filename);
		 end if;
	  end;
   end On_Open_Button_Clicked;

   function Can_Hide
     (Dialog : access Gsl_Dialog_Record)
     return Boolean
   is
	  Response : Default_Dialog.Response_Type;
	  use Default_Dialog;
   begin
	  Response := Default_Dialog.Get_Response (Dialog);
	  Put_Line ("Close:" & Response_Type'Image (Response));

	  if (Default_Dialog.Get_Response (Dialog) 
		  = Default_Dialog.Response_Okay) then
		 -- FIX: execute script
		 null;
	  end if;

	  -- FIX: free the filename
	  --Ada.Strings.Unbounded.Free (Dialog.Filename'Access);
      return True;
   end Can_Hide;

   procedure Create
     (Dialog  :    out Gsl_Dialog_Access)
   is
   begin
	  Dialog := new Gsl_Dialog_Record;
	  Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Gsl_Dialog_Record'class)
   is
	  Center_Box : Gtk.Box.Gtk_Vbox;
	  Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
	  Default_Dialog.Initialize (Dialog,
								 -"Execute GSL Script", 
								 Default_Dialog.Button_Okay_Cancel);

	  Gtk.Text.Gtk_New (Dialog.Text_Area);
	  Gtk.Text.Set_Editable (Dialog.Text_Area, True);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Dialog.Text_Area);

	  Center_Box := Default_Dialog.Get_Center_Box (Dialog);
	  Gtk.Box.Pack_Start (Center_Box, Scrolled_Window,
						  Expand => True, Fill => True, 
						  Padding => DEFAULT_SPACING);
      Gtk.Box.Reorder_Child (Center_Box, Scrolled_Window, 0);

	  Default_Dialog.Add (Dialog, 
						  New_Button (-"Open", On_Open_Button_Clicked'Access));
   end;

end Giant.Gsl_Dialog;
