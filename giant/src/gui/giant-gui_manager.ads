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
--  $RCSfile: giant-gui_manager.ads,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003/06/20 16:47:35 $
--
--  Stores the window records. Handles the controller updates. Provides
--  a facade for the gui.
--
--  Pattern:
--    ADT
--

--  with Giant.Graph_Widgets;
--  with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Window;
with Giant.Vis;
with Giant.Vis_Windows;

package Giant.Gui_Manager is

--     type Crosshair_Action_Callback is access function
--       (Widget   : access Graph_Widgets.Graph_Widget_Record'Class;
--        Action   : in     Graph_Widgets.Notifications.Crosshair_Action_Type;
--        Location : in     Vis.Logic.Vector_2d)
--       return Boolean;

--     generic
--        type Data_Type is private;
--     package Crosshair_Action_Package is

--        procedure Execute (Custom_Object : in Data_Type);

--     private

--        Custom_Object : Data_Type;

--     end Crosshair_Action_Package;

--     Pending_Action : Crosshair_Action_Callback;

   ---------------------------------------------------------------------------
   --  Main Application
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Shows the main window.
   --
   procedure Show;

   ---------------------------------------------------------------------------
   --  Quits the application.
   --
   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean;

   procedure Set_Project_Loaded
     (Loaded : in Boolean);

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window_Name : in String;
      Name        : in String);

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Add_Subgraph
     (Name : in String);

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String);

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String);

   function Close
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean)
     return Boolean;

   function Get_Open_Window (Name : in String)
     return Graph_Window.Graph_Window_Access;

   function Is_Window_Open
     (Name : in String)
     return Boolean;

   procedure Open
     (Visual_Window : Vis_Windows.Visual_Window_Access);

   function Remove_Window
     (Name : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

end Giant.Gui_Manager;

