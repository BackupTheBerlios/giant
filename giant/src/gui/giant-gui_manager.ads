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
--  $RCSfile: giant-gui_manager.ads,v $, $Revision: 1.23 $
--  $Author: squig $
--  $Date: 2003/06/30 15:46:28 $
--
--  Stores the window records. Handles the controller updates. Provides
--  a facade for the gui.
--
--  Pattern:
--    ADT
--

with Giant.Graph_Window;
with Giant.Progress_Dialog;
with Giant.Vis_Windows;

package Giant.Gui_Manager is

   type For_Each_Graph_Window_Type is access procedure
     (Window : in Graph_Window.Graph_Window_Access);

   ---------------------------------------------------------------------------
   --  Main Application
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Shows the main window.
   procedure Show;

   ---------------------------------------------------------------------------
   --  Hides the main window.
   --
   --  Parameters:
   --    Ask_For_Confirmation - If True and project is modified, ask
   --    the user to save the project
   --  Returns:
   --    True, if the windows were closed; False, if the user
   --    cancelled.
   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   function Create_Gsl_Progress_Dialog
     return Progress_Dialog.Progress_Dialog_Access;

   procedure Initialize_Project;

   procedure Set_Status
     (Text : in String);

   ---------------------------------------------------------------------------
   --  Pins
   ---------------------------------------------------------------------------

   procedure Add_Pin
     (Window_Name : in String;
      Name        : in String);

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Update_Pin
     (Window_Name : in String;
      Name        : in String);

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

   procedure Update_Selection
     (Window_Name : in String;
      Name        : in String);

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

   procedure Update_Subgraph
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   procedure Update_Vis_Style
     (Window_Name : in String);

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Update_Zoom_Level
     (Window_Name : in String);

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String);

   function Close
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Invokes Call_Procedure for each open graph window.
   procedure For_Each
     (Call_Procedure : in For_Each_Graph_Window_Type);

   ---------------------------------------------------------------------------
   --  Returns the window with Name, if window is open; null, if
   --  window is not open.
   function Get_Open_Window (Name : in String)
     return Graph_Window.Graph_Window_Access;

   function Is_Window_Open
     (Name : in String)
     return Boolean;

   procedure Open
     (Visual_Window : Vis_Windows.Visual_Window_Access);

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

   ---------------------------------------------------------------------------
   --  Enables or disables the global action mode for all graph windows.
   procedure Set_Action_Mode
     (Activate : in Boolean);

   procedure Update_Window
     (Name : in String);

end Giant.Gui_Manager;

