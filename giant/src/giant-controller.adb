
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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/06/17 20:28:40 $
--

with Giant.Graph_Lib;
with Giant.Gui_Manager;
With Giant.Logger;
with Giant.Vis_Windows;

package body Giant.Controller is

   package Logger is new Giant.Logger("giant.controller");

   ---------------------------------------------------------------------------
   --  Creates a new project.
   --
   --  See:
   --    Project_Management.Create_New_Empty_Project
   procedure Create_Project
     (Name              : in String;
      Project_Directory : in String;
      IML_Graph_File    : in String)
   is
      Checksum : Integer := 0;
   begin
      -- create graph
      Giant.Graph_Lib.Create (IML_Graph_File);

      Valid_Names.Verify_Standard_Name (Name);

      -- create project
      Current_Project := Projects.Create_Empty_Project
        (Name, Project_Directory, IML_Graph_File, Checksum);
   end Create_Project;

   function Get_Project
     return Projects.Project_Access
   is
   begin
      return Current_Project;
   end;

   function Close_Window
     (Name : in String)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access;
   begin
      Window := Projects.Get_Visualisation_Window (Current_Project, Name);
      return Gui_Manager.Close (Window);
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
      Gui_Manager.Remove_Window (Name);
      Projects.Remove_Visualisation_Window
        (Current_Project, Name);
      null;
   end Remove_Window;

   procedure Show
   is
   begin
      Gui_Manager.Show;
   end Show;

end Giant.Controller;

