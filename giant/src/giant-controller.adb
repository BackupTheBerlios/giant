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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.28 $
--  $Author: squig $
--  $Date: 2003/06/27 14:34:55 $
--

with Ada.Strings.Unbounded;

with String_Lists;

with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gui_Manager;
With Giant.Logger;
With Giant.Node_Annotations;
with Giant.Vis_Windows;

package body Giant.Controller is

   package Logger is new Giant.Logger("giant.controller");

   type Subgraph_Operation_Type is access function
     (Left        : in Graph_Lib.Subgraphs.Subgraph;
      Right       : in Graph_Lib.Subgraphs.Subgraph;
      Target_Name : in String)
     return Graph_Lib.Subgraphs.Subgraph;

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   procedure Initialize_Project
   is
   begin
      Project_Loaded := True;

      Gui_Manager.Initialize_Project;
      Logger.Info (-"Project initialized");
   end;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (not Gui_Manager.Close_Project (Ask_For_Confirmation)) then
         return False;
      end if;

      Logger.Info (-"Closing current project");

      Project_Loaded := False;
      --FIX: Current_Project := null;

      Projects.Deallocate_Project_Deep (Current_Project);
      Graph_Lib.Unload;

      return True;
   end;

   procedure Create_Project
     (Filename             : in String;
      Graph_Filename       : in String)
   is
      Checksum : Integer;
   begin
      --  create graph
      Graph_Lib.Load (Graph_Filename);
      Checksum := Graph_Lib.Get_Graph_Hash;

      Logger.Info (-"Creating project " & Filename);

      --  create project
      Current_Project := Projects.Create_Empty_Project_For_File
        (Filename, Graph_Filename, Checksum);

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

   function Has_Project_Changed
     return Boolean
   is
   begin
      return Project_Changed;
   end Has_Project_Changed;

   function Is_Project_Loaded
     return Boolean
   is
   begin
      return Project_Loaded;
   end Is_Project_Loaded;

   procedure Open_Project
     (Filename : in String)
   is
      Graph_Filename : Ada.Strings.Unbounded.Unbounded_String;
      Checksum : Integer;
   begin
      Projects.Get_Bauhaus_IML_Graph_Data_File
        (Filename, Graph_Filename, Checksum);

      --  create graph
      Logger.Info (-"Loading graph "
                   & Ada.Strings.Unbounded.To_String (Graph_Filename));
      Giant.Graph_Lib.Load (Ada.Strings.Unbounded.To_String (Graph_Filename));

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
      Projects.Store_Whole_Project_As_For_File (Current_Project, Filename);
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
     (Ask_For_Confirmation : in Boolean := True)
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
   --  Node Annotations
   ---------------------------------------------------------------------------

   function Get_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
     return String
   is
   begin
      if (Is_Node_Annotated (Node)) then
         return Node_Annotations.Get_Annotation_Text
           (Projects.Get_Node_Annotations (Current_Project), Node);
      else
         return "";
      end if;
   end;

   function Is_Node_Annotated
     (Node : in Graph_Lib.Node_Id)
     return Boolean
   is
   begin
      return Node_Annotations.Is_Annotated
        (Projects.Get_Node_Annotations (Current_Project), Node);
   end;

   procedure Set_Node_Annotation
     (Node : in Graph_Lib.Node_Id;
      Text : in String)
   is
   begin
      if (Is_Node_Annotated (Node)) then
         Node_Annotations.Change_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node, Text);
      else
         Node_Annotations.Add_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node, Text);
      end if;
   end;

   procedure Remove_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
   is
   begin
      if (Is_Node_Annotated (Node)) then
         Node_Annotations.Remove_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node);
      end if;
   end;

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String := "Name")
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Graph_Lib.Selections.Create (Name);
   begin
      Vis_Windows.Add_Selection (Window, Selection);
      Gui_Manager.Add_Selection (Window_Name, Name);
   end Create_Selection;

   procedure Create_Selection_From_Subgraph
     (Subgraph_Name : in String;
      Window_Name   : in String;
      Name          : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Projects.Get_Subgraph (Current_Project, Subgraph_Name);
      Selection : Graph_Lib.Selections.Selection
        := Graph_Lib.Subgraphs.Create_Selection (Subgraph, Name);
   begin
      Vis_Windows.Add_Selection (Window, Selection);
      Gui_Manager.Add_Selection (Window_Name, Name);
   end Create_Selection_From_Subgraph;

   procedure Duplicate_Selection
     (Window_Name : in String;
      Source_Name : in String;
      Target_Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Source : Graph_Lib.Selections.Selection
        := Vis_Windows.Get_Selection (Window, Source_Name);
      Target : Graph_Lib.Selections.Selection
        := Graph_Lib.Selections.Clone (Source, Target_Name);
   begin
      Vis_Windows.Add_Selection (Window, Target);
      Gui_Manager.Add_Selection (Window_Name, Target_Name);
   end Duplicate_Selection;

   procedure Hide_Selection
     (Window_Name    : in String;
      Selection_Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Vis_Windows.Fade_Out_Selection (Window, Selection_Name);
   end;

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (Gui_Manager.Remove_Selection (Window_Name, Name)) then
         Vis_Windows.Remove_Selection (Window, Name);
         return True;
      else
         return False;
      end if;
   end Remove_Selection;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Vis_Windows.Change_Selection_Name
        (Window, Old_Name, New_Name);
      Gui_Manager.Rename_Selection (Window_Name, Old_Name, New_Name);
   end Rename_Selection;

   procedure Show_All_Selections
     (Window_Name    : in String)
   is
   begin
      null;
   end;

   procedure Show_Selection
     (Window_Name    : in String;
      Selection_Name : in String)
   is
   begin
      null;
   end;

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
      Graph_Lib.Subgraphs.Add_Edge_Set (Subgraph, Graph_Lib.Get_All_Edges);
      Graph_Lib.Subgraphs.Add_Node_Set (Subgraph, Graph_Lib.Get_All_Nodes);

      Projects.Add_Subgraph (Current_Project, Subgraph);
      Gui_Manager.Add_Subgraph (Unique_Name);
   end Create_Subgraph;

   procedure Create_Subgraph_From_Selection
     (Window_Name    : in String;
      Selection_Name : in String;
      Name           : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Vis_Windows.Get_Selection (Window, Selection_Name);
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Graph_Lib.Subgraphs.Create (Name, Selection);
   begin
      Projects.Add_Subgraph (Current_Project, Subgraph);
      Gui_Manager.Add_Subgraph (Name);
   end Create_Subgraph_From_Selection;

   procedure Duplicate_Subgraph
     (Source_Name : in String;
      Target_Name : in String)
   is
      Source : Graph_Lib.Subgraphs.Subgraph
        := Projects.Get_Subgraph (Current_Project, Source_Name);
      Target : Graph_Lib.Subgraphs.Subgraph
        := Graph_Lib.Subgraphs.Clone (Source, Target_Name);
   begin
      Projects.Add_Subgraph (Current_Project, Target);
      Gui_Manager.Add_Subgraph (Target_Name);
   end Duplicate_Subgraph;

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Subgraph (Name)) then
         Projects.Remove_Subgraph (Current_Project, Name);
         return True;
      else
         return False;
      end if;
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

   procedure Set_Subgraph_Highlight_Status
     (Name   : in String;
      Status : in Projects.Subgraph_Highlight_Status)
   is
   begin
      Projects.Change_Highlight_Status (Current_Project, Name,
                                        Status);
      Gui_Manager.Update_Subgraph (Name);
   end Set_Subgraph_Highlight_Status;

   procedure Subgraph_Operation
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String;
      Operation   : in Subgraph_Operation_Type)
   is
      Left : Graph_Lib.Subgraphs.Subgraph;
      Right : Graph_Lib.Subgraphs.Subgraph;
      Target : Graph_Lib.Subgraphs.Subgraph;
   begin
      Left := Projects.Get_Subgraph (Current_Project, Left_Name);
      Right := Projects.Get_Subgraph (Current_Project, Right_Name);

      --  operation
      Target := Operation (Left, Right, Target_Name);

      --  add to project
      Projects.Add_Subgraph (Current_Project, Target);
      Gui_Manager.Add_Subgraph (Target_Name);
   end;

   procedure Subgraph_Difference
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Symetric_Difference'Access);
   end;

   procedure Subgraph_Intersection
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Intersection'Access);
   end;

   procedure Subgraph_Union
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Union'Access);
   end;

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Removed : Boolean;
   begin
      if (Gui_Manager.Close (Name, Ask_For_Confirmation)) then
         Projects.Free_Memory_For_Vis_Window (Current_Project, Name);

         if (Projects.Does_Vis_Window_Exist (Current_Project, Name)) then
            Gui_Manager.Update_Window (Name);
         else
            --  the window was never saved
            Removed :=
              Gui_Manager.Remove_Window (Name, Ask_For_Confirmation => False);
         end if;
         return True;
      end if;
      return False;
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
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Name);
   begin
      Gui_Manager.Open (Window);
   end Open_Window;

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Window (Name, Ask_For_Confirmation)) then
         if (Projects.Is_Vis_Window_Memory_Loaded (Current_Project, Name)) then
            Projects.Free_Memory_For_Vis_Window (Current_Project, Name);
         end if;

         Projects.Remove_Visualisation_Window (Current_Project, Name);
         return True;
      end if;

      return False;
   end Remove_Window;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String)
   is
      --  make sure the window is loaded
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Old_Name);
   begin
      Projects.Change_Vis_Window_Name
        (Current_Project, Old_Name, New_Name);

      Gui_Manager.Rename_Window (Old_Name, New_Name);
   end Rename_Window;

   procedure Save_Window
     (Name : in String)
   is
      --  make sure the window is loaded
      Window : Vis_Windows.Visual_Window_Access;
   begin
      if (Projects.Is_Vis_Window_Memory_Loaded (Current_Project, Name)) then
         Window := Projects.Get_Visualisation_Window (Current_Project, Name);
         Projects.Store_Single_Visualisation_Window (Current_Project, Name);
         Logger.Info ("Saved window " & Name);
      end if;
   end Save_Window;

end Giant.Controller;

