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
--  $RCSfile: giant-dialogs.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/24 10:43:05 $
--

with Gtk.Box;
with Gtkada.Pixmaps;

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
        (Dialog, Pixmaps.Confirmation_Xpm, Message);

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
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Pixmaps.Error_Xpm, Message);

      Default_Dialog.Show_Modal (Dialog);

      Default_Dialog.Destroy (Dialog);
   end Show_Error_Dialog;

   procedure Show_Info_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error")
   is
      Dialog : Default_Dialog.Default_Dialog_Access;
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Create (Dialog, Title, Default_Dialog.Button_Okay);
      Box := Default_Dialog.Add_Icon_Box
        (Dialog, Pixmaps.Information_Xpm, Message);

      Default_Dialog.Show_Modal (Dialog);

      Default_Dialog.Destroy (Dialog);
   end Show_Error_Dialog;

   function Show_Input_Dialog
     (Message         : in String;
      Title           : in String                                   := -"Giant Input";
      Default_Input   : in String                                   := "";
      Input_Validator : in Gtk_Object_Input_Dialog.Input_Validator_Type := null)
      return String
   is
   begin
      return Gtk_Object_Input_Dialog.Show (Message, Title, Default_Input,
                                           Input_Validator, null);
   end Show_Input_Dialog;

end Giant.Dialogs;
