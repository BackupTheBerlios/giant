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
--  $RCSfile: giant-set_operation_dialog.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/03 14:45:52 $
--

with Glib;
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Window;

with Giant.Default_Dialog;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Set_Operation_Dialog is

   procedure Create
     (Dialog  :    out Set_Operation_Dialog_Acess)
   is
   begin
      Dialog := new Set_Operation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog  : access Set_Operation_Dialog_Record'Class)
   is
      Box : Gtk.Box.Gtk_Vbox;
   begin
      Default_Dialog.Initialize (Dialog, -"Set Operation",
								 Default_Dialog.Button_Okay_Cancel);

      Box := Default_Dialog.Get_Center_Box (Dialog);
   end;

   function Can_Hide
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean
   is
      -- the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
      use Default_Dialog;
   begin
      Response := Default_Dialog.Get_Response (Dialog);

      if (Default_Dialog.Get_Response (Dialog)
          = Default_Dialog.Response_Cancel) then
         -- the cancel button was pressed
         Set_Operation_Dialog_Callback.Emit_By_Name (Dialog, "cancelled");
      end if;

      return False;
   end Can_Hide;

   function Get_Activity_Mode
     (Dialog        : access Set_Operation_Dialog_Record)
     return Boolean
   is
   begin
      return Gtk.Progress_Bar.Get_Activity_Mode (Dialog.Progress_Bar);
   end Get_Activity_Mode;

   procedure Set_Activity_Mode
     (Dialog        : access Set_Operation_Dialog_Record;
      Activity_Mode : in     Boolean)
   is
   begin
      Gtk.Progress_Bar.Set_Activity_Mode (Dialog.Progress_Bar, Activity_Mode);
   end Set_Activity_Mode;

   procedure Set_Lower
     (Dialog : access Set_Operation_Dialog_Record;
      Lower  : in     Float)
   is
   begin
      Gtk.Adjustment.Set_Lower (Dialog.Progress_Bar_Adjustment,
                                Glib.Gfloat (Lower));
   end Set_Lower;

   procedure Set_Percentage
     (Dialog     : access Set_Operation_Dialog_Record;
      Percentage : in     Float)
   is
   begin
      Gtk.Progress_Bar.Set_Percentage (Dialog.Progress_Bar,
                                       Glib.Gfloat (Percentage));
   end Set_Percentage;

   procedure Set_Progress_Text
     (Dialog : access Set_Operation_Dialog_Record;
      Text   : in     String)
   is
   begin
      Gtk.Progress_Bar.Set_Show_Text (Dialog.Progress_Bar, True);
      Gtk.Progress_Bar.Set_Format_String (Dialog.Progress_Bar, Text);
   end Set_Progress_Text;

   procedure Set_Upper (Dialog : access Set_Operation_Dialog_Record;
                        Upper  : in     Float)
   is
   begin
      Gtk.Adjustment.Set_Upper (Dialog.Progress_Bar_Adjustment,
                                Glib.Gfloat (Upper));
   end Set_Upper;

   procedure Set_Value (Dialog : access Set_Operation_Dialog_Record;
                        Value  : in     Float)
   is
   begin
      Gtk.Adjustment.Set_Value (Dialog.Progress_Bar_Adjustment,
                                Glib.Gfloat (Value));
   end Set_Value;

end Giant.Set_Operation_Dialog;
