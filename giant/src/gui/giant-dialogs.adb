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
--  $RCSfile: giant-dialogs.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/22 21:54:21 $
--

with Gtk.Box;

package body Giant.Dialogs is

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Default_Dialog.Button_Type := Default_Dialog.Button_Yes_No)
     return Default_Dialog.Response_Type
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
	  Response : Default_Dialog.Response_Type;
   begin
      Default_Dialog.Create (Dialog, -"Giant Question", Buttons);
      Box := Default_Dialog.Add_Icon_Box
		(Dialog, "gnome-question.xpm", Message);

      Default_Dialog.Show_Modal (Dialog);

	  Response := Default_Dialog.Get_Response (Dialog);
      Default_Dialog.Destroy (Dialog);

      return Response;
   end Show_Confirmation_Dialog;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error")
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Create (Dialog, Title, Default_Dialog.Button_Close);
      Box := Default_Dialog.Add_Icon_Box (Dialog, "gnome-error.xpm", Message);

      Default_Dialog.Show_Modal (Dialog);

      Default_Dialog.Destroy (Dialog);
   end Show_Error_Dialog;


   function Show_Input_Dialog
     (Message		  : in String;
	  Title			  : in String							 := -"Giant Input";
	  Default_Input	  : in String							 := "";
	  Input_Validator : in Input_Dialog.Input_Validator_Type := null)
      return String
   is
	  use type Default_Dialog.Response_Type;

      Dialog : Input_Dialog.Input_Dialog_Access;
   begin
      Input_Dialog.Create (Dialog, Title, Message, Input_Validator);
      Input_Dialog.Set_Text (Dialog, Default_Input);

      Input_Dialog.Show_Modal (Dialog);

      if (Input_Dialog.Get_Response (Dialog) 
		  = Default_Dialog.Response_Okay) then
         declare
            S : constant String := Input_Dialog.Get_Text (Dialog);
         begin
            Input_Dialog.Destroy (Dialog);
            return S;
         end;
      else
         Input_Dialog.Destroy (Dialog);
         return "";
      end if;
   end Show_Input_Dialog;

end Giant.Dialogs;
