
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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.10 $
--  $Author: squig $
--  $Date: 2003/06/18 18:40:37 $
--

with Ada.Strings.Unbounded;

with String_Lists;

with Giant.Graph_Lib;
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
      --FIX: Current_Project := null;
      null;
   end;

   procedure Initialize_Project
   is
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  initialize main window data
      List := Projects.Get_All_Visualisation_Window_Names
        (Current_Project);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Gui_Manager.Add_Window (Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);

      Gui_Manager.Set_Project_Loaded (True);
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

   procedure Open_Project
     (Filename : in String)
   is
   begin
      Logger.Info (-"Closing current project");
      Close_Project;

      Logger.Info (-"Opening project " & Filename);
      Current_Project := Projects.Load_Project_File (Filename);
   end;

   procedure Save_Project
   is
   begin
      Projects.Store_Whole_Project (Current_Project);
   end Save_Project;

   procedure Save_Project
     (Filename : in String)
   is
   begin
      Projects.Store_Whole_Project_As (Current_Project, Filename, "");
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

   procedure Show_Gui
   is
   begin
      Gui_Manager.Show;
   end Show_Gui;

   function Hide_Gui
     (Ask_For_Confirmation: Boolean := True)
     return Boolean
   is
   begin
      return Gui_Manager.Hide (Ask_For_Confirmation);
   end Hide_Gui;

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name : in String;
      Ask_For_Confirmation: Boolean := True)
     return Boolean
   is
   begin
      return Gui_Manager.Close (Name, Ask_For_Confirmation);
   end Close_Window;

   procedure Create_Window
     (Name : in String := "Unknown")
   is
      Window : Vis_Windows.Visual_Window_Access;
      I : Integer := 1;
   begin
      --  try to find a unique name for window
      while (True) loop
         declare
            Custom_Name : String := Name & Integer'Image (I);
         begin
            Logger.Debug ("creating project: trying name " & Custom_Name);

            if (not Projects.Does_Vis_Window_Exist
                (Current_Project, Custom_Name)) then
               Window := Vis_Windows.Create_New (Custom_Name);
               Projects.Add_Visualisation_Window (Current_Project, Window);
               Gui_Manager.Add_Window (Custom_Name);
               Open_Window (Custom_Name);
               return;
            end if;
         end;
         I := I + 1;
      end loop;
   end Create_Window;

   procedure Open_Window
     (Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access;
   begin
      Window := Projects.Get_Visualisation_Window (Current_Project, Name);
      Gui_Manager.Open (Window);
   end Open_Window;

   procedure Remove_Window
     (Name : in String)
   is
   begin
      if (Gui_Manager.Is_Window_Open (Name)) then
         if (not Gui_Manager.Close (Name, Ask_For_Confirmation => False)) then
            --  user has aborted close
            return;
         end if;
      end if;

      Gui_Manager.Remove_Window (Name);
      Projects.Remove_Visualisation_Window
        (Current_Project, Name);
   end Remove_Window;

end Giant.Controller;

