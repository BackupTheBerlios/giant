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
--  $RCSfile: giant-gsl_dialog-callbacks.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
--  Provides callbacks for gsl dialogs.
--

with Gdk.Event;
with Gtk.Widget;

package Giant.Gsl_Dialog.Callbacks is

   ---------------------------------------------------------------------------
   --  File Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_File_Insert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_New
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Open
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Open_External
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Revert
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Save
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_File_Save_As
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Edit Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edit_Clear
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Copy
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Cut
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edit_Paste
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Button Callbacks
   ---------------------------------------------------------------------------

   procedure On_Run_Button_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Other Callbacks
   ---------------------------------------------------------------------------

   function On_Focus
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean;

end Giant.Gsl_Dialog.Callbacks;
