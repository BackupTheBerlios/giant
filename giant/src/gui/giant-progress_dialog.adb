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
--  $RCSfile: giant-progress_dialog.adb,v $, $Revision: 1.7 $
--  $Author: schulzgt $
--  $Date: 2003/06/20 15:05:52 $
--

with Glib;
with Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Object;
with Gtk.Progress_Bar;
with Gtk.Window;
with Gtkada.Types;

with Interfaces.C.Strings;
with System;

with Giant.Default_Dialog;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Progress_Dialog is

   Class_Record : System.Address := System.Null_Address;

   Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => Interfaces.C.Strings.New_String ("cancelled"));

   package Progress_Dialog_Callback is new
     Gtk.Handlers.Callback (Progress_Dialog_Record);

   procedure Create
     (Dialog  :    out Progress_Dialog_Access;
      Title   : in     String;
      Message : in     String)

   is
   begin
      Dialog := new Progress_Dialog_Record;
      Initialize (Dialog, Title, Message);
   end Create;

   procedure Initialize
     (Dialog  : access Progress_Dialog_Record'Class;
      Title   : in     String;
      Message : in     String)
   is
      Box : Gtk.Box.Gtk_Vbox;
   begin
      Default_Dialog.Initialize (Dialog, Title, Default_Dialog.Button_Cancel);

      -- provide signals
      Gtk.Object.Initialize_Class_Record
        (Dialog, Signals, Class_Record);

      Box := Default_Dialog.Get_Center_Box (Dialog);

      Gtk.Label.Gtk_New (Dialog.Progress_Label, Message);
      Gtk.Box.Pack_Start (Box, Dialog.Progress_Label, Expand => False,
                         Fill => False, Padding => DEFAULT_SPACING);

      Gtk.Adjustment.Gtk_New (Dialog.Progress_Bar_Adjustment,
                              Value => 0.0, Lower => 0.0,
                              Upper => 100.0, Step_Increment => 1.0,
                              Page_Increment => 1.0, Page_Size => 1.0);

      Gtk.Progress_Bar.Gtk_New (Dialog.Progress_Bar,
                                Dialog.Progress_Bar_Adjustment);
      Gtk.Progress_Bar.Set_Activity_Blocks (Dialog.Progress_Bar, 10);
      Gtk.Progress_Bar.Set_Activity_Step (Dialog.Progress_Bar, 1);
      Gtk.Box.Pack_Start (Box, Dialog.Progress_Bar, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);
   end;

   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
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
         Progress_Dialog_Callback.Emit_By_Name (Dialog, "cancelled");
      end if;

      return False;
   end Can_Hide;

   function Get_Activity_Mode
     (Dialog        : access Progress_Dialog_Record)
     return Boolean
   is
   begin
      return Gtk.Progress_Bar.Get_Activity_Mode (Dialog.Progress_Bar);
   end Get_Activity_Mode;

   procedure Set_Activity_Mode
     (Dialog        : access Progress_Dialog_Record;
      Activity_Mode : in     Boolean)
   is
   begin
      Gtk.Progress_Bar.Set_Activity_Mode (Dialog.Progress_Bar, Activity_Mode);
   end Set_Activity_Mode;

   procedure Set_Lower
     (Dialog : access Progress_Dialog_Record;
      Lower  : in     Float)
   is
   begin
      Gtk.Adjustment.Set_Lower (Dialog.Progress_Bar_Adjustment,
                                Glib.Gfloat (Lower));
   end Set_Lower;

   procedure Set_Percentage
     (Dialog     : access Progress_Dialog_Record;
      Percentage : in     Float)
   is
   begin
      Gtk.Progress_Bar.Set_Percentage (Dialog.Progress_Bar,
                                       Glib.Gfloat (Percentage));
   end Set_Percentage;

   procedure Set_Progress_Text
     (Dialog : access Progress_Dialog_Record;
      Text   : in     String)
   is
   begin
      Gtk.Progress_Bar.Set_Show_Text (Dialog.Progress_Bar, True);
      Gtk.Progress_Bar.Set_Format_String (Dialog.Progress_Bar, Text);
   end Set_Progress_Text;

   procedure Set_Upper (Dialog : access Progress_Dialog_Record;
                        Upper  : in     Float)
   is
   begin
      Gtk.Adjustment.Set_Upper (Dialog.Progress_Bar_Adjustment,
                                Glib.Gfloat (Upper));
   end Set_Upper;

   procedure Set_Value (Dialog : access Progress_Dialog_Record;
                        Value  : in     Float)
   is
      Upper_Bound : Float;
      Mod_Value : Float := Value;
   begin
      --  adjust value
      Upper_Bound := abs Float (Gtk.Adjustment.Get_Upper
                                (Dialog.Progress_Bar_Adjustment));
      if (Upper_Bound = Float(0)) then
         Gtk.Adjustment.Set_Value (Dialog.Progress_Bar_Adjustment,
                                   Glib.Gfloat (0));
      else
         while (Mod_Value > Upper_Bound) loop
            Mod_Value := Mod_Value - Upper_Bound;
         end loop;

         Gtk.Adjustment.Set_Value (Dialog.Progress_Bar_Adjustment,
                                   Glib.Gfloat (Mod_Value));
      end if;
   end Set_Value;

end Giant.Progress_Dialog;
