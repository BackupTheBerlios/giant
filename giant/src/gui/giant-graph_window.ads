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
--  $RCSfile: giant-graph_window.ads,v $, $Revision: 1.16 $
--  $Author: squig $
--  $Date: 2003/06/29 21:23:23 $
--
------------------------------------------------------------------------------
--
-- Provides the graph window.
--

with Gdk.Event;
with Gtk.Clist;
with Gtk.Combo;
with Gtk.Gentry;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Option_Menu;
with Gtk.Paned;
with Gtk.Style;
with Gtk.Window;

with Giant.Graph_Widgets;
with Giant.Gui_Utils;
with Giant.Mini_Maps;
with Giant.Vis;
with Giant.Vis_Windows;

package Giant.Graph_Window is

   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with private;

   type Graph_Window_Access is access all Graph_Window_Record'Class;

   ---------------------------------------------------------------------------
   --  Package: Actions
   ---------------------------------------------------------------------------

   package Actions is

      type Graph_Window_Action_Type is abstract tagged private;

      type Graph_Window_Action_Access is
        access all Graph_Window_Action_Type'Class;

      procedure Cancel
        (Action : access Graph_Window_Action_Type)
         is abstract;

      procedure Destroy
        (Action : access Graph_Window_Action_Type);

      procedure Execute
        (Action   : access Graph_Window_Action_Type;
         Window   : access Graph_Window.Graph_Window_Record'Class;
         Event    : in     Gdk.Event.Gdk_Event_Button;
         Location : in     Vis.Logic.Vector_2d)
         is abstract;

   private

      type Graph_Window_Action_Type is abstract tagged null record;

   end Actions;

   function Close
     (Window               : access Graph_Window_Record;
      Ask_For_Confirmation : in     Boolean)
     return Boolean;

   procedure Create
     (Window        :    out Graph_Window_Access;
      Visual_Window : in     Vis_Windows.Visual_Window_Access);

   procedure Initialize
     (Window : access Graph_Window_Record'Class);

   function Get_Vis_Window
     (Window : access Graph_Window_Record)
     return Vis_Windows.Visual_Window_Access;

   procedure Update_Title
     (Window : access Graph_Window_Record);

   procedure Set_Global_Action_Mode
     (Widget : access Graph_Window_Record;
      Enable : in     Boolean);

   ---------------------------------------------------------------------------
   --  Pin Methods
   ---------------------------------------------------------------------------

   procedure Add_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   function Get_Selected_Pin
     (Window : access Graph_Window_Record)
     return String;

   procedure Update_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Remove_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   ---------------------------------------------------------------------------
   --  Selection Methods
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Update_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Remove_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

private
   type Style_Array_Type is array (Vis_Windows.Selection_Highlight_Status)
     of Gtk.Style.Gtk_Style;

   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with record
        Split_Pane : Gtk.Paned.Gtk_Hpaned;
        Pin_List : Gui_Utils.String_Clists.Giant_Data_Clist;
        Pin_List_Menu : Gtk.Menu.Gtk_Menu;
        Selection_List : Gui_Utils.String_Clists.Giant_Data_Clist;
        Selection_List_Menu : Gtk.Menu.Gtk_Menu;
        Vis_Style_Menu : Gtk.Option_Menu.Gtk_Option_Menu;
        Zoom_Combo : Gtk.Combo.Gtk_Combo;
        Zoom_Entry : Gtk.Gentry.Gtk_Entry;

        Graph : Graph_Widgets.Graph_Widget;
        Mini_Map : Mini_Maps.Mini_Map;

        Is_Modified : Boolean := True;

        --  the action that is currently pending
        Local_Action : Actions.Graph_Window_Action_Access
          := null;

        --  the data record from projects
        Visual_Window : Vis_Windows.Visual_Window_Access;

        --  the selection highlight color cell styles
        Styles : Style_Array_Type := (others => null);
     end record;

end Giant.Graph_Window;
