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
--  $RCSfile: giant-gsl_dialog.adb,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003/08/15 16:37:18 $
--

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

with Glib; use type Glib.Gint;
with Gtkada.File_Selection;
with Gtk.Adjustment;
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
with Giant.Logger;
with Giant.Main_Window;

package body Giant.Gsl_Dialog is

   package Logger is new Giant.Logger("giant.gsl_dialog");

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
     when E: others =>
        Logger.Warn ("error reading file: " & Filename);
        Logger.Error (E);

        Controller.Handle_IO_Exception (E, Filename);
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
     when E : others =>
        Logger.Warn ("error writing file: " & Filename);
        Logger.Error (E);

        Controller.Handle_IO_Exception (E, Filename);
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
            Set_Filename (Dialog, Filename);
            return Write (Dialog, Filename);
         end if;
      end;

      return True;
   end Show_Save_As_Dialog;

   ---------------------------------------------------------------------------
   --  Returns:
   --    False, if user cancelled or data was not modified; True, otherwise
   function Save_Changes
     (Dialog : access Gsl_Dialog_Record'Class)
      return Boolean
   is
      use type Default_Dialog.Response_Type;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Response : Default_Dialog.Response_Type;
   begin
      if (Dialog.Text_Has_Changed) then
         Response := Dialogs.Show_Confirmation_Dialog
           (-"The script has changed. The saved script will be executed. Save changes?",
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

   procedure On_Can_Close_Project
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      if (not Save_Changes (Dialog)) then
         Main_Window.Cancel_Close_Project;
      end if;
   end On_Can_Close_Project;

   procedure On_Open_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      declare
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"Open GSL Script",
            Ada.Strings.Unbounded.To_String (Dialog.Filename),
            Dir_Only => False, Must_Exist => True);
         Success : Boolean;
      begin
         if (Filename /= "") then
            if (Save_Changes (Dialog)) then
               Set_Filename (Dialog, Filename);
               Success := Read (Dialog, Filename);
            end if;
         end if;
      end;
   end On_Open_Button_Clicked;

   procedure On_Run_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
   begin
      if (Dialog.Filename
          /= Ada.Strings.Unbounded.Null_Unbounded_String) then
         declare
            Filename : String
              := Ada.Strings.Unbounded.To_String (Dialog.Filename);
         begin
            Controller.Execute_GSL (Filename);

            --  the script was executed successfully
            --  close the dialog?
         exception
           when E : others =>
              Logger.Warn ("error executing gsl script: " & Filename);
              Logger.Error (E);

              Dialogs.Show_Error_Dialog
                (-"Error during execution"
                 & " (" & Ada.Exceptions.Exception_Message (E)
                 & ").");
              -- FIX: if syntax error: jump to position
         end;
      end if;
   end On_Run_Button_Clicked;

   procedure On_Save_As_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Success : Boolean;
   begin
      Success := Show_Save_As_Dialog (Dialog);
   end On_Save_As_Button_Clicked;

   procedure On_Text_Area_Changed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dialog : constant Gsl_Dialog_Access := Gsl_Dialog_Access (Source);
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Id : Gtk.Status_Bar.Message_Id;
   begin
      Dialog.Text_Has_Changed := True;

      Adjustment := Gtk.Text.Get_Hadj (Dialog.Text_Area);
      Id := Gtk.Status_Bar.Push (Dialog.Location_Bar, 1,
                                 ":" & Glib.Gint'Image
                                 (Gtk.Text.Get_Position
                                  (Dialog.Text_Area)));
   end On_Text_Area_Changed;

   function Can_Hide
     (Dialog : access Gsl_Dialog_Record)
     return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Default_Dialog.Response_Type;
   begin
      if (not Save_Changes (Dialog)) then
         return False;
      end if;

      --  the dialog was cancelled
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

      Center_Box : Gtk.Box.Gtk_Vbox;
      --  holds multiple status bars
      Status_Box : Gtk.Box.Gtk_Hbox;

      Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"Execute GSL Script",
                                 Default_Dialog.Button_Close);

      --  vertical center box
      Gtk.Box.Gtk_New_Vbox (Center_Box);
      Set_Center_Widget (Dialog, Center_Box);

      --  text area
      Gtk.Text.Gtk_New (Dialog.Text_Area);
      Gtk.Text.Set_Editable (Dialog.Text_Area, True);
      Gtk.Text.Set_USize (Dialog.Text_Area, -1, 250);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Dialog.Text_Area);
      Gtk.Box.Pack_Start (Center_Box, Scrolled_Window, Expand => False,
                          Fill => True, Padding => 0);

      --  status bars
      Gtk.Box.Gtk_New_Hbox (Status_Box);
      Gtk.Box.Pack_End (Center_Box, Status_Box, Expand => False, Fill => True,
                        Padding => 0);

      Gtk.Status_Bar.Gtk_New (Dialog.Filename_Bar);
      Gtk.Box.Pack_Start (Status_Box, Dialog.Filename_Bar, Expand => True,
                          Fill => True, Padding => 0);

      Gtk.Status_Bar.Gtk_New (Dialog.Location_Bar);
      Gtk.Box.Pack_Start (Status_Box, Dialog.Location_Bar, Expand => False,
                          Fill => True, Padding => 0);

      -- buttons
      Add_Button (Dialog, New_Button (-"Run",
                                      On_Run_Button_Clicked'Access,
                                      Dialog));
      Add_Button (Dialog, New_Button (-"Save As...",
                                      On_Save_As_Button_Clicked'Access,
                                      Dialog));
      Add_Button (Dialog, New_Button (-"Open...",
                                      On_Open_Button_Clicked'Access,
                                      Dialog));

      Set_Default (Dialog, Dialog.Text_Area);

      Widget_Callback.Object_Connect
        (Dialog.Text_Area, "changed",
         Widget_Callback.To_Marshaller
         (On_Text_Area_Changed'Access), Dialog);

      --  connect close project
      Main_Window.Connect_Can_Close_Project
        (On_Can_Close_Project'Access, Dialog);
   end;

   function Get_Filename
     (Dialog   : access Gsl_Dialog_Record)
     return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Dialog.Filename);
   end Get_Filename;

   procedure Set_Filename
     (Dialog   : access Gsl_Dialog_Record;
      Filename : in     String)
   is
      Id : Gtk.Status_Bar.Message_Id;
   begin
      Dialog.Filename
        := Ada.Strings.Unbounded.To_Unbounded_String (Filename);
      Id := Gtk.Status_Bar.Push (Dialog.Filename_Bar, 1, Filename);
   end Set_Filename;

end Giant.Gsl_Dialog;
