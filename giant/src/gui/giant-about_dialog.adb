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
--  $RCSfile: giant-about_dialog.adb,v $, $Revision: 1.10 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--

with Ada.IO_Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

with Glib; use type Glib.Gint;
with Gtkada.File_Selection;
with Gtk.Box;
with Gtk.Button;
with Gtk.Old_Editable;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Text;
with Gtk.Widget;

with Giant.Constants;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Gui_Utils; use Giant.Gui_Utils;

package body Giant.About_Dialog is

   procedure Initialize
     (Dialog : access About_Dialog_Record'class);


   ---------------------------------------------------------------------------
   --  File Operations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was read successfully; False, otherwise
   procedure Show_File
     (Text_Area : access Gtk.Old_Editable.Gtk_Old_Editable_Record'Class;
      Filename : String;
      Alt_Text : String)
   is
      In_File : Ada.Text_Io.File_Type;
      Line : String(1..1024);
      Last : Integer;
      Position : Glib.Gint := 0;
   begin
      Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File, Filename);

      -- Fix: Clear
      --Gtk.Old_Editable.Set_Position (Dialog.Text_Area, Glib.Gint (-1));
      Position := Gtk.Old_Editable.Get_Position (Text_Area);

      while (not Ada.Text_Io.End_Of_File (In_File)) loop
         Ada.Text_Io.Get_Line (In_File, Line, Last);
         Gtk.Old_Editable.Insert_Text (Text_Area, Line(1..Last) & ASCII.LF,
                                   Position);
      end loop;

      Ada.Text_IO.Close (In_File);
   exception
      when others =>
         declare
            Position : Glib.Gint := Gtk.Old_Editable.Get_Position (Text_Area);
         begin
            Gtk.Old_Editable.Insert_Text (Text_Area, Alt_Text, Position);
         end;
   end Show_File;

   procedure Append_Author
     (Text_Area : access Gtk.Old_Editable.Gtk_Old_Editable_Record'Class;
      Author : String)
   is
      Position : Glib.Gint := Gtk.Old_Editable.Get_Position (Text_Area);
   begin
      Gtk.Old_Editable.Insert_Text (Text_Area, Author, Position);
      Gtk.Old_Editable.Insert_Text (Text_Area, ASCII.LF & "" & ASCII.LF, Position);
   end;

   ---------------------------------------------------------------------------
   --  Initializer
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog  :    out About_Dialog_Access)
   is
   begin
      Dialog := new About_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access About_Dialog_Record'class)
   is
      Box : Gtk.Box.Gtk_Hbox;
      Label : Gtk.Label.Gtk_Label;
      Notebook : Gtk.Notebook.Gtk_Notebook;
      Text_Area : Gtk.Text.Gtk_Text;
      Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Position : Glib.Gint := 0;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"About GIANT" & " (Version "
                                 & Constants.Version & ")",
                                 Default_Dialog.Button_Close);

      --  logo and notebook
      Gtk.Notebook.Gtk_New (Notebook);
      Gtk.Notebook.Popup_Enable (Notebook);
      Box := Add_Icon_Box (Dialog, "giant-logo.xpm", Notebook);

      --  about
      Gtk.Label.Gtk_New (Label, "GIANT" & ASCII.LF & "" & ASCII.LF
                         & "  Graphical IML Analysis and Navigation Tool  ");
      Gtk.Notebook.Append_Page_Menu (Notebook, Label,
                                     New_Label (-"About"),
                                     New_Label (-"About"));

      --  authors
      Gtk.Text.Gtk_New (Text_Area);
      Gtk.Text.Set_Editable (Text_Area, False);
      Gtk.Text.Set_Line_Wrap (Text_Area, False);
      Gtk.Text.Set_Word_Wrap (Text_Area, False);
      Append_Author (Text_Area, "Martin Schwienbacher (Config, Projects)");
      Append_Author (Text_Area, "Gerrit Schulz (Gsl)");
      Append_Author (Text_Area, "Steffen Pingel (Gui, Controller)");
      Append_Author (Text_Area, "Philipp Häuser (Handbook)");
      Append_Author (Text_Area, "Oliver Kopp (Graph_Lib, Layouts)");
      Append_Author (Text_Area, "Steffen Keul (Visualisation and More)");

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Automatic);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Text_Area);
      Gtk.Notebook.Append_Page_Menu (Notebook, Scrolled_Window,
                                     New_Label (-"Authors"),
                                     New_Label (-"Authors"));

      --  license
      Gtk.Text.Gtk_New (Text_Area);
      Gtk.Text.Set_Line_Wrap (Text_Area, False);
      Gtk.Text.Set_Word_Wrap (Text_Area, False);
      Gtk.Text.Set_Editable (Text_Area, False);
      Show_File (Text_Area, "COPYING", -"License file could not be read. See http://gnu.org/licenses/gpl.html");

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Automatic);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Text_Area);
      Gtk.Notebook.Append_Page_Menu (Notebook, Scrolled_Window,
                                     New_Label (-"License"),
                                     New_Label (-"License"));
   end;

   procedure Show
   is
      Dialog : About_Dialog_Access := null;
   begin
      Create (Dialog);
      Show_All (Dialog);
   end;

end Giant.About_Dialog;
