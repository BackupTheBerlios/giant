------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-make_room_dialog.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Ada.IO_Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

with Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Box;
with Gtk.Label;
with Gtk.Spin_Button;

with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.Make_Room_Dialog is

   function Can_Hide
     (Dialog : access Make_Room_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then
         -- the okay button was pressed
         -- FIX: make room
         null;
      end if;

      return True;
   end Can_Hide;

   procedure Create
     (Dialog : out Make_Room_Dialog_Access)
   is
   begin
      Dialog := new Make_Room_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Make_Room_Dialog_Record'class)
   is
      Pixel_Button : Gtk.Spin_Button.Gtk_Spin_Button;
      Label : Gtk.Label.Gtk_Label;
      Center_Box : Gtk.Box.Gtk_Vbox;
      Pixel_Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Initialize (Dialog, -"Make Room",
                                 Default_Dialog.Button_Okay_Cancel);

      --  center box
      Center_Box := Get_Center_Box (Dialog);
      Label := New_Label(-"Create an empty space by shifting away all nodes from the selected point.");
      Gtk.Label.Set_Line_Wrap (Label, True);

      Gtk.Box.Pack_Start (Center_Box, Label, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      --  spin button
      Gtk.Adjustment.Gtk_New (Dialog.Pixel_Adjustment,
                              Value => 0.0, Lower => 0.0,
                              Upper => 5000.0, Step_Increment => 1.0,
                              Page_Increment => 1.0, Page_Size => 1.0);
      Gtk.Spin_Button.Gtk_New (Pixel_Button, Dialog.Pixel_Adjustment,
                               Climb_Rate => 1.0, The_Digits => 0);
      Gtk.Spin_Button.Set_Numeric (Pixel_Button, True);
      Gtk.Spin_Button.Set_Value (Pixel_Button, DEFAULT_VALUE);

      --  pixel label
      Gtk.Box.Gtk_New_Hbox (Pixel_Box);
      Gtk.Box.Pack_Start (Center_Box, Pixel_Box, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      Gtk.Box.Pack_Start (Pixel_Box, Pixel_Button, Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Pixel_Box, New_Label(-"Pixels"), Expand => False,
                          Fill => False, Padding => DEFAULT_SPACING);

      Set_Default (Dialog, Pixel_Button);
   end;

   function Show
     return Float
   is
      Dialog : Make_Room_Dialog_Access;
      Value : Float;
   begin
      Create (Dialog);
      Show_Modal (Dialog);
      if (Is_Response_Okay (Dialog)) then
         Value := Float (Gtk.Adjustment.Get_Value (Dialog.Pixel_Adjustment));
      else
         Value := -1.0;
      end if;
      Destroy (Dialog);
      return Value;
   end Show;


end Giant.Make_Room_Dialog;
