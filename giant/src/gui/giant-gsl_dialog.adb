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
--  $RCSfile: giant-gsl_dialog.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/06/22 21:54:21 $
--

with Ada.IO_Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

with Glib; use type Glib.Gint;
with Gtkada.File_Selection;
with Gtk.Box;
with Gtk.Button;
with Gtk.Editable;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Scrolled_Window;
with Gtk.Text;
with Gtk.Widget;

with Giant.Controller;
with Giant.Dialogs;
with Giant.Gui_Utils;

package body Giant.Gsl_Dialog is

   ---------------------------------------------------------------------------
   --  File Operations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was read successfully; False, otherwise
   function Read
     (Dialog   : access Gsl_Dialog_Record'Class;
      Filename : String)
     return Boolean
   is
      In_File : Ada.Text_Io.File_Type;
      Line : String(1..1024);
      Last : Integer;
      Position : Glib.Gint := 0;
   begin
      Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File, Filename);

      -- Fix: Clear
      --Gtk.Text.Set_Position (Dialog.Text_Area, Glib.Gint (-1));
      Position := Gtk.Text.Get_Position (Dialog.Text_Area);

      while (not Ada.Text_Io.End_Of_File (In_File)) loop
         Ada.Text_Io.Get_Line (In_File, Line, Last);
         Gtk.Text.Insert_Text (Dialog.Text_Area, Line(1..Last) & ASCII.LF,
                               Position);
      end loop;

      Ada.Text_IO.Close (In_File);

      Dialog.Text_Has_Changed := False;
      return True;
   exception
      when others =>
         Dialogs.Show_Error_Dialog(-"Could not read file: " & Filename);
         return False;
   end Read;

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was written successfully; False, otherwise
   function Write
     (Dialog   : access Gsl_Dialog_Record'Class;
      Filename : String)
     return Boolean
   is
      Out_File : Ada.Text_Io.File_Type;
      Text : String := Gtk.Text.Get_Chars (Dialog.Text_Area);
   begin
      begin
         Ada.Text_IO.Open (Out_File, Ada.Text_IO.Out_File, Filename);
      exception
         when Ada.IO_Exceptions.Name_Error =>
           Ada.Text_IO.Create (Out_File, Ada.Text_IO.Out_File, Filename);
      end;

      Ada.Text_Io.Put (Out_File, Text);

      Ada.Text_IO.Close (Out_File);

      Dialog.Text_Has_Changed := False;
      return True;
   exception
      when others =>
         Dialogs.Show_Error_Dialog(-"Could not write file: " & Filename);
         return False;
   end Write;

   ---------------------------------------------------------------------------
   --  File Dialogs
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was saved or dialog was cancelled; False, otherwise
   function Show_Save_As_Dialog
     (Dialog   : access Gsl_Dialog_Record'Class)
     return Boolean
   is
   begin
      declare
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"Save GSL Script",
            Ada.Strings.Unbounded.To_String (Dialog.Filename),
            Dir_Only => False, Must_Exist => False);
      begin
         if (Filename /= "") then
            Dialog.Filename
              := Ada.Strings.Unbounded.To_Unbounded_String (Filename);
            return Write (Dialog, Filename);
         end if;
      end;

      return True;
   end Show_Save_As_Dialog;

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file can be purged; False, otherwise
   function Save_Unchanged
     (Dialog : access Gsl_Dialog_Record'Class)
      return Boolean
   is
      use type Default_Dialog.Response_Type;
      use Ada.Strings.Unbounded;

      Response : Default_Dialog.Response_Type;
   begin
      if (Dialog.Text_Has_Changed) then
         Response := Dialogs.Show_Confirmation_Dialog
           (-"The text has changed. Save changes?",
            Default_Dialog.Button_Yes_No_Cancel);
         if (Response = Default_Dialog.Response_Yes) then
            if (Dialog.Filename
                = Ada.Strings.Unbounded.Null_Unbounded_String) then
               return Show_Save_As_Dialog (Dialog);
            else
               return Write (Dialog, Ada.Strings.Unbounded.To_String
                             (Dialog.Filename));
            end if;
         elsif (Response = Default_Dialog.Response_Cancel) then
            return False;
         end if;
      end if;

      return True;
   end;

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

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
         Success : Boolean;
      begin
         if (Filename /= "") then
            if (Save_Unchanged (Dialog)) then
               Dialog.Filename
                 := Ada.Strings.Unbounded.To_Unbounded_String (Filename);
               Success := Read (Dialog, Filename);
            end if;
         end if;
      end;
   end On_Open_Button_Clicked;

   procedure On_Save_As_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Gsl_Dialog_Access;
      Success : Boolean;
   begin
      Dialog := Gsl_Dialog_Access (Gtk.Widget.Get_Toplevel
                                   (Gtk.Widget.Gtk_Widget (Source)));
      Success := Show_Save_As_Dialog (Dialog);
   end On_Save_As_Button_Clicked;

   procedure On_Text_Area_Changed
     (Source : access Gtk.Editable.Gtk_Editable_Record'Class)
   is
      Dialog : Gsl_Dialog_Access;
   begin
      Dialog := Gsl_Dialog_Access (Gtk.Widget.Get_Toplevel
                                   (Gtk.Widget.Gtk_Widget (Source)));
      Dialog.Text_Has_Changed := True;
   end On_Text_Area_Changed;

   function Can_Hide
     (Dialog : access Gsl_Dialog_Record)
     return Boolean
   is
      -- the reason for closing the dialog
      Response : Default_Dialog.Response_Type;
      use Default_Dialog;
   begin
      Response := Get_Response (Dialog);

      if (not Save_Unchanged (Dialog)) then
         return False;
      end if;

      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then
         -- the okay button was pressed
         declare
            Script : String := Gtk.Text.Get_Text (Dialog.Text_Area);
         begin
            Controller.Execute_GSL (Script);
         end;
      end if;

      return True;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializer
   ---------------------------------------------------------------------------

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
      use Giant.Gui_Utils;

      Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"Execute GSL Script",
                                 Default_Dialog.Button_Okay_Cancel);

      -- text area
      Gtk.Text.Gtk_New (Dialog.Text_Area);
      Gtk.Text.Set_Editable (Dialog.Text_Area, True);
      Gtk.Text.Set_USize (Dialog.Text_Area, -1, 250);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Dialog.Text_Area);

      Set_Center_Widget (Dialog, Scrolled_Window);

      -- buttons
      Add_Button (Dialog, New_Button (-"Open...",
                                      On_Open_Button_Clicked'Access));
      Add_Button (Dialog, New_Button (-"Save As...",
                                      On_Save_As_Button_Clicked'Access));

      Set_Default (Dialog, Dialog.Text_Area);

      Editable_Callback.Connect
        (Dialog.Text_Area, "changed",
         Editable_Callback.To_Marshaller (On_Text_Area_Changed'Access));
   end;

end Giant.Gsl_Dialog;
