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
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-evolution.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--
------------------------------------------------------------------------------


with Ada.Text_IO; use Ada.Text_IO;

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
with Giant.Progress_Dialog;

with Giant_Test.Concurrent_Calculations;
with Giant_Test.Iterative_Calculations;

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
      Dialog          : Giant.Progress_Dialog.Progress_Dialog_Access;
   begin
      Calculation := Concurrent_Calculations.Create (Start_Count, 100);
      Giant.Progress_Dialog.Create
        (Dialog  => Dialog,
         Title   => "Concurrent_Calculation",
         Message => "This is a message.");

      Put ("Create Dialog ");
      Concurrent_Calculations.Start_Calculation
        (Individual => Calculation,
         Started    => Started,
         Dialog     => Dialog);
      Put_Line ("done.");

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
      Dialog          : Giant.Progress_Dialog.Progress_Dialog_Access;
   begin
      Calculation := Iterative_Calculations.Create (Start_Count, 100);

      Giant.Progress_Dialog.Create
        (Dialog  => Dialog,
         Title   => "Iterative_Calculation",
         Message => "This is a message.");

      Iterative_Calculations.Start_Calculation
        (Individual => Calculation,
         Started    => Started,
         Dialog     => Dialog);

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
