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
--  $RCSfile: giant-input_dialog.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/06/24 10:50:12 $
--

with Gtk.Button;
with Gtk.Box;
with Gtk.Widget;
with Gtkada.Pixmaps;

package body Giant.Input_Dialog is

   procedure Create
     (Dialog          :    out Input_Dialog_Access;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type)
   is
   begin
      Dialog := new Input_Dialog_Record;
      Initialize (Dialog, Title, Message, Input_Validator, Custom_Data);
   end Create;

   procedure Initialize
     (Dialog          : access Input_Dialog_Record'class;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type)
   is
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Initialize (Dialog, Title,
                                 Default_Dialog.Button_Okay_Cancel);

      Dialog.Input_Validator := Input_Validator;
      Dialog.Custom_Data := Custom_Data;

      Box := Add_Icon_Box (Dialog, Gtkada.Pixmaps.Confirmation_Xpm, Message);

      Gtk.Gentry.Gtk_New (Dialog.Input);
      Gtk.Box.Add (Box, Dialog.Input);

      Gtk.Gentry.Set_Flags (Dialog.Input, Gtk.Widget.Can_Default);
      Gtk.Gentry.Grab_Default (Dialog.Input);
   end Initialize;

   function Can_Hide
     (Dialog : access Input_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog)
          = Default_Dialog.Response_Okay) then
         if (Dialog.Input_Validator /= null) then
            return Dialog.Input_Validator (Get_Text (Dialog),
                                           Dialog.Custom_Data);
         end if;
      end if;

      return True;
   end Can_Hide;

   function Get_Text
     (Dialog : access Input_Dialog_Record)
     return String
   is
      Text : constant String := Gtk.Gentry.Get_Chars (Dialog.Input);
   begin
      return Text;
   end Get_Text;

   procedure Set_Text
     (Dialog : access Input_Dialog_Record;
      Text   : in     String)
   is
   begin
      Gtk.Gentry.Set_Text (Dialog.Input, Text);
   end Set_Text;

   function Show
     (Message         : in String;
      Title           : in String               := -"Giant Input";
      Default_Input   : in String               := "";
      Input_Validator : in Input_Validator_Type := null;
      Custom_Data     : in Data_Type)
      return String
   is
      use type Default_Dialog.Response_Type;

      Dialog : Input_Dialog_Access;
   begin
      Input_Dialog.Create (Dialog, Title, Message, Input_Validator,
                           Custom_Data);
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
   end Show;

end Giant.Input_Dialog;
