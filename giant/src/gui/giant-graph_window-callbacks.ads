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
--  $RCSfile: giant-graph_window-callbacks.ads,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
--  Provides callbacks for graph window.
--

with Gtk.Arguments;
with Gtk.Widget;

with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Menu_Factory;

package Giant.Graph_Window.Callbacks is

   ---------------------------------------------------------------------------
   --  Background Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Background_Create_Pin
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Make_Room
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Background_Select_All
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Background_Select_Nothing
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Edge Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edge_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Edge_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edge_Show_Target
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Edge_Zoom
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Graph Widget Callbacks
   ---------------------------------------------------------------------------

   procedure On_Action_Mode_Button_Pressed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Background_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Edge_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Node_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action);

   procedure On_Selection_Changed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Selection_Change_Action);

   ---------------------------------------------------------------------------
   --  Node Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Node_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Node_Show_Info
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Node_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Node_Annotate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Selection Callbacks
   ---------------------------------------------------------------------------

   procedure On_Apply_Layout
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Selection_List_Set_Operation
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Selection_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event);

   procedure On_Selection_Zoom_To
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

end Giant.Graph_Window.Callbacks;
