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
--  $RCSfile: giant-main_window.ads,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
--  Provides the main window. The main window is only instanciated once.
--

with Gdk.Event;
with Gtk.Widget;
with Gtk.Window;

with Giant.Graph_Window;
with Giant.Gui_Manager;
with Giant.Gui_Utils;
with Giant.Vis;
with Giant.Vis_Windows;
with Giant.Valid_Names;

package Giant.Main_Window is

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with private;

   type Main_Window_Access is access all Main_Window_Record'Class;

   ---------------------------------------------------------------------------
   --  Window Methods
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String);

   procedure Update_Window
     (Name : in String);

   procedure Remove_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------
   procedure Add_Subgraph
     (Name : in String);

   procedure Update_Subgraph
     (Name : in String);

   procedure Remove_Subgraph
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Called by listeners of the "can_close_project" signal to
   --  cancel the close operation.
   --
   --  This is a ugly workaround because it seems to be impossible to
   --  define custom signals that have a return value.
   procedure Cancel_Close_Project;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Connect_Can_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Connect_Close_Project
     (Callback : in     Gui_Utils.Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Sets windows visible.
   --
   procedure Show;

   ---------------------------------------------------------------------------
   --  Hides the application.
   --
   --  Return:
   --    True, if window was hidden.
   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean;

   procedure Initialize_Project;

   procedure Set_Status
     (Text : in String);

   procedure Set_Graph_Filename
     (Text : in String);

   procedure Update_Column_Sizes;

private

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record
     with null record;

end Giant.Main_Window;
