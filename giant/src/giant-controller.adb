
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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003/06/19 16:38:06 $
--

with Ada.Strings.Unbounded;

with String_Lists;

with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gui_Manager;
With Giant.Logger;
with Giant.Vis_Windows;

package body Giant.Controller is

   package Logger is new Giant.Logger("giant.controller");

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   procedure Close_Project
   is
   begin
      Project_Loaded := False;

      --FIX: Current_Project := null;

      Gui_Manager.Set_Project_Loaded (Project_Loaded);
   end;

   procedure Initialize_Project
   is
   begin
      Project_Loaded := True;

      Gui_Manager.Set_Project_Loaded (Project_Loaded);
      Logger.Info (-"Project initialized");
   end;

   procedure Create_Project
     (Filename           : in String;
      IML_Graph_Filename : in String)
   is
      Checksum : Integer;
   begin
      Close_Project;

      --  create graph
      Giant.Graph_Lib.Create (IML_Graph_Filename);
      Checksum := Graph_Lib.Get_Graph_Hash;

      Logger.Info (-"Creating project " & Filename);

      --  create project
      Current_Project := Projects.Create_Empty_Project_For_File
        (Filename, IML_Graph_Filename, Checksum);

      --  update application
      Initialize_Project;
   end Create_Project;

   function Get_Project
     return Projects.Project_Access
   is
   begin
      return Current_Project;
   end;

   function Get_Unique_Name
     (Name : in String := "Unknown")
      return String
   is
      I : Natural := 1;
   begin
      --  try the plain name first
      if (not Projects.Exists_Name (Current_Project, Name)) then
         return Name;
      end if;

      while (True) loop
         declare
            Unique_Name : String := Name & Integer'Image (I);
         begin
            Logger.Debug ("Get_Unique_Name: trying name " & Unique_Name);

            if (not Projects.Exists_Name (Current_Project, Unique_Name)) then
               return Unique_Name;
            end if;
         end;
         I := I + 1;
      end loop;

      --  this can never happen, we will get a constrained error instead
      --  when I runs out of bounds
      return "";
   end;

   function Is_Project_Loaded
     return Boolean
   is
   begin
      return Project_Loaded;
   end Is_Project_Loaded;

   procedure Open_Project
     (Filename : in String)
   is
   begin
      Logger.Info (-"Closing current project");
      Close_Project;

      Logger.Info (-"Opening project " & Filename);
      Current_Project := Projects.Load_Project_File (Filename);

      --  update application
      Initialize_Project;
   end;

   procedure Save_Project
   is
   begin
      Projects.Store_Whole_Project (Current_Project);
      Logger.Info (-"Project saved");
   end Save_Project;

   procedure Save_Project
     (Filename : in String)
   is
   begin
      Projects.Store_Whole_Project_As (Current_Project, Filename, "");
      Logger.Info (-"Project saved");
   end Save_Project;

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   procedure Execute_GSL
     (Script : in String)
   is
   begin
      -- FIX: execute script
      null;
   end Execute_GSL;

   ---------------------------------------------------------------------------
   --  GUI
   ---------------------------------------------------------------------------

   function Hide_Gui
     (Ask_For_Confirmation: Boolean := True)
     return Boolean
   is
   begin
      return Gui_Manager.Hide (Ask_For_Confirmation);
   end Hide_Gui;

   procedure Show_Gui
   is
   begin
      Gui_Manager.Show;
   end Show_Gui;

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Create_Subgraph
     (Name : in String := "Unknown")
   is
      Subgraph : Graph_Lib.Subgraphs.Subgraph;
      Unique_Name : String := Get_Unique_Name (Name);
   begin
      Subgraph := Graph_Lib.Subgraphs.Create (Unique_Name);
      Projects.Add_Subgraph (Current_Project, Subgraph);
      Gui_Manager.Add_Subgraph (Unique_Name);
   end Create_Subgraph;

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Subgraph (Name)) then
         Projects.Remove_Subgraph (Current_Project, Name);
         return True;
      end if;
      return False;
   end Remove_Subgraph;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String)
   is
   begin
      Projects.Change_Subgraph_Name
        (Current_Project, Old_Name, New_Name);

      Gui_Manager.Rename_Subgraph (Old_Name, New_Name);
   end Rename_Subgraph;

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      return Gui_Manager.Close (Name, Ask_For_Confirmation);
   end Close_Window;

   procedure Create_Window
     (Name : in String := "Unknown")
   is
      Window : Vis_Windows.Visual_Window_Access;
      Unique_Name : String := Get_Unique_Name (Name);
   begin
      Window := Vis_Windows.Create_New (Unique_Name);
      Projects.Add_Visualisation_Window (Current_Project, Window);
      Gui_Manager.Add_Window (Unique_Name);
      Open_Window (Unique_Name);
   end Create_Window;

   procedure Open_Window
     (Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access;
   begin
      Window := Projects.Get_Visualisation_Window (Current_Project, Name);
      Gui_Manager.Open (Window);
   end Open_Window;

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Window (Name)) then
         Projects.Remove_Visualisation_Window
           (Current_Project, Name);
         return True;
      end if;

      return False;
   end Remove_Window;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String)
   is
   begin
      Projects.Change_Vis_Window_Name
        (Current_Project, Old_Name, New_Name);

      Gui_Manager.Rename_Window (Old_Name, New_Name);
   end Rename_Window;

end Giant.Controller;

