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
--  $RCSfile: giant-main_window.ads,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/06/23 19:19:34 $
--
--  Provides the main window. The main window is only instanciated once.
--
--  Pattern:
--    ADO
--

with Giant.Graph_Window;
with Giant.Vis_Windows;
with Giant.Valid_Names;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Crosshair;

package Giant.Main_Window is

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

   procedure Set_Project_Loaded
     (Loaded : in Boolean);

private

   ---------------------------------------------------------------------------
   --  Subgraph Crosshair
   ---------------------------------------------------------------------------

   type Create_Selection_Action_Type (Name_Length : Positive) is new
     Gui_Manager.Crosshair.Crosshair_Action_Type with record
        Subgraph_Name : String(1 .. Name_Length);
     end record;

   type Create_Selection_Action_Access is
     access all Create_Selection_Action_Type'Class;

   procedure Cancel
     (Action : access Create_Selection_Action_Type);

   procedure Execute
     (Action : access Create_Selection_Action_Type;
      Window : access Graph_Window.Graph_Window_Record'Class);

end Giant.Main_Window;
