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
--  $RCSfile: giant-gui_manager.adb,v $, $Revision: 1.9 $
--  $Author: squig $
--  $Date: 2003/06/19 19:37:05 $
--

with Ada.Strings.Unbounded;

with Gdk.Threads;
with Gtk.Main;

with Lists;
with String_Lists;

with Giant.Controller;
with Giant.Main_Window;
with Giant.Graph_Window;
with Giant.Projects;

package body Giant.Gui_Manager is

   Gui_Initialized : Boolean := False;

   package Graph_Window_Lists is new Lists (Graph_Window.Graph_Window_Access);
   Open_Windows : Graph_Window_Lists.List := Graph_Window_Lists.Create;

   ---------------------------------------------------------------------------
   --  Initialize Helper
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Initializes the main window.
   procedure Initialize_Project
   is
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  add windows
      List := Projects.Get_All_Visualisation_Window_Names
        (Controller.Get_Project);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Add_Window (Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);

      --  add subgraphs
      List := Projects.Get_All_Subgraphs (Controller.Get_Project);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Add_Subgraph (Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);
   end;

   procedure Initialize_Graph_Window
     (Window : access Graph_Window.Graph_Window_Record'Class)
   is
      Vis_Window : Vis_Windows.Visual_Window_Access
        := Graph_Window.Get_Vis_Window (Window);

      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  add pins
      List := Vis_Windows.Get_All_Pins (Vis_Window);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Graph_Window.Add_Pin (Window, Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (List);

      --  add selections
   end;

   ---------------------------------------------------------------------------
   --  Main Application
   ---------------------------------------------------------------------------

   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean
   is
   begin
      if (Main_Window.Hide (Ask_For_Confirmation)) then
         Gtk.Main.Main_Quit;
         return True;
      end if;
      return False;
   end Hide;

   procedure Show
   is
   begin
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;
      Gdk.Threads.Init;

      Gdk.Threads.Enter;

      --  initialize windows
      Main_Window.Show;

      --  initialize state
      Set_Project_Loaded (Controller.Is_Project_Loaded);

      -- main loop
      Gui_Initialized := True;
      Gtk.Main.Main;
      Gui_Initialized := False;

      Gdk.Threads.Leave;
   end Show;

   procedure Set_Project_Loaded
     (Loaded : in Boolean)
   is
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Main_Window.Set_Project_Loaded (Loaded);

      if (Loaded) then
         Initialize_Project;
      end if;
   end Set_Project_Loaded;

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Add_Subgraph
     (Name : in String)
   is
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Main_Window.Add_Subgraph (Name);
   end Add_Subgraph;

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (not Gui_Initialized) then
         return True;
      end if;

      -- FIX: Ask for confirmation

      Main_Window.Remove_Subgraph (Name);
      return True;
   end Remove_Subgraph;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String)
   is
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Main_Window.Remove_Subgraph (Old_Name);
      Main_Window.Add_Subgraph (New_Name);
   end Rename_Subgraph;

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String)
   is
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Main_Window.Add_Window (Name);
   end Add_Window;

   function Close
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean)
     return Boolean
   is
      use type Graph_Window.Graph_Window_Access;

      Closed : Boolean;
      Window : Graph_Window.Graph_Window_Access;
   begin
      if (not Gui_Initialized) then
         return True;
      end if;

      Window := Get_Open_Window (Name);
      if (Window = Graph_Window.Null_Graph_Window) then
         --  window is not open
         return True;
      end if;

      if (Ask_For_Confirmation) then
         Closed := Graph_Window.Close (Window);
      else
         Graph_Window.Hide (Window);
         Closed := True;
      end if;

      if (Closed) then
         Graph_Window_Lists.DeleteItem (Open_Windows, Window);

         --  deallocate
         Graph_Window.Destroy (Window);

         --  update status
         Main_Window.Update_Window (Name);
      end if;

      return Closed;
   end Close;

   function Get_Open_Window (Name : in String)
     return Graph_Window.Graph_Window_Access
   is
      Iterator : Graph_Window_Lists.ListIter;
      Window : Graph_Window.Graph_Window_Access;
   begin
      if (not Gui_Initialized) then
         return Graph_Window.Null_Graph_Window;
      end if;

      Iterator := Graph_Window_Lists.MakeListIter (Open_Windows);
      while Graph_Window_Lists.More (Iterator) loop
         Graph_Window_Lists.Next (Iterator, Window);
         if (Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window))
             = Name) then
            return Window;
         end if;
      end loop;

      return Graph_Window.Null_Graph_Window;
   end Get_Open_Window;

   function Is_Window_Open
     (Name : in String)
     return Boolean
   is
      use type Graph_Window.Graph_Window_Access;
   begin
      if (not Gui_Initialized) then
         return False;
      end if;

      return (Get_Open_Window (Name) /= Graph_Window.Null_Graph_Window);
   end Is_Window_Open;

   procedure Open (Visual_Window : Vis_Windows.Visual_Window_Access)
   is
      use type Graph_Window.Graph_Window_Access;

      Window : Graph_Window.Graph_Window_Access;
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Window := Get_Open_Window (Vis_Windows.Get_Name (Visual_Window));
      if (Window /= Graph_Window.Null_Graph_Window) then
         --  window is already open, focus
         Graph_Window.Grab_Focus (Window);
         return;
      end if;

      Graph_Window.Create (Window, Visual_Window);
      Graph_Window_Lists.Attach (Open_Windows, Window);

      Initialize_Graph_Window (Window);

      Graph_Window.Show_All (Window);

      --  update status
      Main_Window.Update_Window (Vis_Windows.Get_Name (Visual_Window));
   end Open;

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (not Gui_Initialized) then
         return True;
      end if;

      if (Is_Window_Open (Name)) then
         if (Close (Name, Ask_For_Confirmation => Ask_For_Confirmation)) then
            --  user has aborted close
            return False;
         end if;
      end if;

      Main_Window.Remove_Window (Name);
      return True;
   end Remove_Window;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String)
   is
      Window : Graph_Window.Graph_Window_Access;
   begin
      if (not Gui_Initialized) then
         return;
      end if;

      Main_Window.Remove_Window (Old_Name);
      Main_Window.Add_Window (New_Name);

      Window := Get_Open_Window (New_Name);
      Graph_Window.Update_Title (Window);
   end Rename_Window;

end Giant.Gui_Manager;
