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
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-evolution.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/02 00:09:07 $
--
------------------------------------------------------------------------------


with Gdk.Event;
with Gdk.Threads;
with Gtk.Dialog;
with Gtk.Progress_Bar;
with Gtk.Label;
with Gtk.Button;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Vbutton_Box;
with Gtk.Window;

with Giant.Default_Logger;
with Giant.Evolutions;

with Giant_Test.Concurrent_Calculations;
with Giant_Test.Iterative_Calculations;
with Giant_Test.Progress_Dialogs;

procedure Giant_Test.Evolution is

   function On_Main_Window_Delete_Event
     (Window : access Gtk.Window.Gtk_Window_Record'Class)
      return Boolean is
   begin
      Gtk.Main.Gtk_Exit (0);
      return False;
   end On_Main_Window_Delete_Event;

   Start_Count : Natural := 1;

   procedure On_Main_Window_Concurrent_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

      Calculation     : Concurrent_Calculations.Counter_Access;
      Started         : Boolean;
      Progress_Dialog : Gtk.Dialog.Gtk_Dialog := null;
      Progress_Bar    : Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : Gtk.Label.Gtk_Label := null;
      Progress_Cancel : Gtk.Button.Gtk_Button := null;
   begin
      Calculation := Concurrent_Calculations.Create (Start_Count, 100);

      Progress_Dialogs.Make_New_Progress_Dialog
        (Progress_Dialog, Progress_Bar, Progress_Cancel);

      Concurrent_Calculations.Start_Calculation
        (Individual      => Calculation,
         Started         => Started,
         Progress_Dialog => Progress_Dialog,
         Progress_Text   => Progress_Text,
         Progress_Bar    => Progress_Bar,
         Progress_Cancel => Progress_Cancel);

      if Started then
         Start_Count := Start_Count + 1;
      else
         Concurrent_Calculations.Finish (Calculation, True);
      end if;
   end On_Main_Window_Concurrent_Clicked;

   procedure On_Main_Window_Iterative_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is

      Calculation     : Iterative_Calculations.Counter_Access;
      Started         : Boolean;
      Progress_Dialog : Gtk.Dialog.Gtk_Dialog := null;
      Progress_Bar    : Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : Gtk.Label.Gtk_Label := null;
      Progress_Cancel : Gtk.Button.Gtk_Button := null;
   begin
      Calculation := Iterative_Calculations.Create (Start_Count, 100);

      Progress_Dialogs.Make_New_Progress_Dialog
        (Progress_Dialog, Progress_Bar, Progress_Cancel);

      Iterative_Calculations.Start_Calculation
        (Individual      => Calculation,
         Started         => Started,
         Progress_Dialog => Progress_Dialog,
         Progress_Text   => Progress_Text,
         Progress_Bar    => Progress_Bar,
         Progress_Cancel => Progress_Cancel);

      if Started then
         Start_Count := Start_Count + 1;
      else
         Iterative_Calculations.Finish (Calculation, True);
      end if;
   end On_Main_Window_Iterative_Clicked;

   procedure On_Main_Window_Quit_Clicked
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Main_Window_Quit_Clicked;


   package Window_Cb is new Gtk.Handlers.Callback
     (Gtk.Window.Gtk_Window_Record);
   package Window_R_Boolean_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record,
      Boolean);
   package Button_Cb is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   User_Area         : Gtk.Vbutton_Box.Gtk_Vbutton_Box;
   Concurrent_Button : Gtk.Button.Gtk_Button;
   Iterative_Button  : Gtk.Button.Gtk_Button;
   Quit_Button       : Gtk.Button.Gtk_Button;
   Main_Window       : Gtk.Window.Gtk_Window;
begin
   Giant.Default_Logger.Init;
   Gdk.Threads.Init;
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gdk.Event.Set_Show_Events (False);

   Gtk.Button.Gtk_New (Concurrent_Button, "Concurrent");
   Gtk.Button.Gtk_New (Iterative_Button, "Iterative");
   Gtk.Button.Gtk_New (Quit_Button, "Quit");

   Gtk.Vbutton_Box.Gtk_New (User_Area);
   Gtk.Vbutton_Box.Add (User_Area, Concurrent_Button);
   Gtk.Vbutton_Box.Add (User_Area, Iterative_Button);
   Gtk.Vbutton_Box.Add (User_Area, Quit_Button);

   Gtk.Window.Gtk_New (Main_Window);
   Gtk.Window.Add (Main_Window, User_Area);

   Window_R_Boolean_Cb.Connect
     (Widget => Main_Window,
      Name   => "delete_event",
      Marsh  => Window_R_Boolean_Cb.To_Marshaller
        (On_Main_Window_Delete_Event'Access));
   Button_Cb.Connect
     (Widget => Concurrent_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller
                  (On_Main_Window_Concurrent_Clicked'Access));
   Button_Cb.Connect
     (Widget => Iterative_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller
                  (On_Main_Window_Iterative_Clicked'Access));
   Button_Cb.Connect
     (Widget => Quit_Button,
      Name   => "clicked",
      Marsh  => Button_Cb.To_Marshaller (On_Main_Window_Quit_Clicked'Access));

   Gtk.Window.Show_All (Main_Window);

   Gdk.Threads.Enter;
   Gtk.Main.Main;
   Gdk.Threads.Leave;
   Giant.Default_Logger.Close;
end Giant_Test.Evolution;
