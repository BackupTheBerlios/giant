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
--  $RCSfile: giant-main_window-actions.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/30 12:08:09 $
--

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

with Gdk.Color;
with Gdk.Event;
with Gdk.Types;
with Glib; use type Glib.Gint;
with Gtk.Box;
with Gtk.Clist;
pragma Elaborate_All (Gtk.Clist);
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Object;
with Gtk.Paned;
with Gtk.Status_Bar;
with Gtk.Style;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Tearoff_Menu_Item;
with Gtkada.File_Selection;
with Gtkada.Types;

with Giant.About_Dialog;
with Giant.Clists;
with Giant.Config.Global_Data;
with Giant.Config_Settings;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Dialogs;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gsl_Dialog;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gui_Utils;
with Giant.Logger;
with Giant.Node_Info_Dialog;
with Giant.Projects;
with Giant.Set_Operation_Dialog;

package body Giant.Main_Window.Actions is

   ---------------------------------------------------------------------------
   --  Create Selection From Subgraph
   ---------------------------------------------------------------------------
   
   function Create 
	 (Subgraph_Name : in String)
	 return Create_Selection_Action_Access
   is
	  Action : Create_Selection_Action_Access;
   begin
	  Action := new Create_Selection_Action_Type (Subgraph_Name'Length);
      Action.Subgraph_Name := Subgraph_Name;
	  return Action;
   end Create;
   
   procedure Cancel
     (Action : access Create_Selection_Action_Type)
   is
   begin
      Destroy (Action);
   end Cancel;

   procedure Execute
     (Action   : access Create_Selection_Action_Type;
      Window   : access Graph_Window.Graph_Window_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d)
   is
   begin
      Controller.Create_Selection_From_Subgraph
        (Action.Subgraph_Name,
         Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window)),
         Action.Subgraph_Name);
   end Execute;

end Giant.Main_Window.Actions;
