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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.53 $
--  $Author: squig $
--  $Date: 2003/07/14 22:28:11 $
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with String_Lists;

with Giant.Config;
with Giant.Config_Settings;
with Giant.Config.Vis_Styles;
with Giant.Dialogs;
with Giant.Evolutions;
with Giant.File_Management;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Widgets;
with Giant.Gui_Manager;
with Giant.Layout_Factory;
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
   --  Application
   ---------------------------------------------------------------------------

   procedure Exit_Application
   is
      use type Gsl.Interpreters.Interpreter;
   begin
      if (Gsl_Interpreter = null) then
         Gsl.Interpreters.Destroy (Gsl_Interpreter);
      end if;
   end Exit_Application;

   function Show_Source
     (Node : in Graph_Lib.Node_Id)
     return Boolean
   is
      use type Graph_Lib.Node_Attribute_Class_Id;
      Iterator : Graph_Lib.Node_Attribute_Iterator;
      Attribute : Graph_Lib.Node_Attribute_Id := null;
   begin
      Iterator := Graph_Lib.Make_Attribute_Iterator (Node);
      while (Graph_Lib.More (Iterator)) loop
         Graph_Lib.Next (Iterator, Attribute);
         if (Graph_Lib.Get_Node_Attribute_Class_Id (Attribute)
             = Graph_Lib.Class_SLoc) then
            --  launch editor
            File_Management.Execute_External_Editor
              (Command  =>
                 Config_Settings.Get_Setting_As_String ("Editor.Source"),
               Filename =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Path_Value
               (Node, Attribute)
               & Graph_Lib.Get_Node_Attribute_SLoc_Filename_Value
               (Node, Attribute),
               Line     =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Line_Value
               (Node, Attribute),
               Column   =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Column_Value
               (Node, Attribute));
            return True;
         end if;
      end loop;

      return False;
   end Show_Source;

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

   procedure Show_Error
     (Message : in String)
   is
   begin
      if (Gui_Manager.Is_Initialized) then
         Dialogs.Show_Error_Dialog (Message);
      else
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
      end if;
   end Show_Error;

   procedure Show_Gui
   is
   begin
      Gui_Manager.Show;
   end Show_Gui;

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   procedure Execute_GSL
     (Filename : in String)
   is
      use type Gsl.Interpreters.Interpreter;

      Started : Boolean;
   begin
      if (Gsl_Interpreter = null) then
         --  lazily instanciate
         Gsl_Interpreter := Gsl.Interpreters.Create_Interpreter;
      end if;

      Gsl.Interpreters.Execute_Script (Gsl_Interpreter, Filename, "");
      Gsl.Interpreters.Start_Calculation
        (Individual => Gsl_Interpreter,
         Started    => Started,
         Dialog     => Gui_Manager.Create_Progress_Dialog
         (-"Executing GSL Script", -"Script is running..."));
      Logger.Info ("Script finished:");
   end Execute_GSL;

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------

   procedure Apply_Layout
     (Window                : in Vis_Windows.Visual_Window_Access;
      Selection             : in Graph_Lib.Selections.Selection;
      Lock                  : in Graph_Widgets.Lock_Type;
      Layout_Name           : in String;
      Position              : in Vis.Logic.Vector_2d              := Vis.Logic.Zero_2d;
      Additional_Parameters : in String)
   is
      Evolution : Evolutions.Evolution_Class_Access;
      Started : Boolean;
   begin
      Layout_Factory.Create (Algorithm => Layout_Name,
                             Selection_To_Layout => Selection,
                             Widget => Vis_Windows.Get_Graph_Widget (Window),
                             Widget_Lock => Lock,
                             Target_Position => Position,
                             Additional_Parameters =>
                               Additional_Parameters,
                             Layout_Evolution => Evolution);
      Evolutions.Start_Calculation (Evolution,
                                    Gui_Manager.Create_Progress_Dialog
                                    (-"Applying Layout",
                                     -"Layout is calculated..."),
                                    Started);
      --  Evolutions.Start_Calculation_Blocked (Evolution);
   end Apply_Layout;

   procedure Apply_Layout
     (Layout_Name           : in String;
      Window_Name           : in String;
      Selection_Name        : in String;
      Position              : in Vis.Logic.Vector_2d;
      Additional_Parameters : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Vis_Windows.Get_Selection (Window, Selection_Name);
      Lock : Graph_Widgets.Lock_Type;
   begin
      Graph_Widgets.Lock_Selection (Vis_Windows.Get_Graph_Widget (Window),
                                    Selection, Lock);
      Apply_Layout (Window, Selection, Lock, Layout_Name, Position,
                    Additional_Parameters);
   end Apply_Layout;

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
      Current_Project := Projects.Null_Project;

      Projects.Deallocate_Project_Deep (Current_Project);
      Graph_Lib.Unload;

      return True;
   end;

   procedure Create_Project
     (Project_Filename : in String;
      Graph_Filename   : in String)
   is
      Checksum : Integer;
   begin
      --  check this now to avoid loading the graph
      if (Projects.Is_Already_A_Project_File_In_Directory
          (File_Management.Return_Dir_Path_For_File_Path (Project_Filename)))
          then
         raise Projects.Directory_Holds_Already_A_Project_File_Exception;
      end if;

      --  create graph
      Graph_Lib.Load (Graph_Filename);
      Checksum := Graph_Lib.Get_Graph_Hash;

      Logger.Info (-"Creating project " & Project_Filename);

      --  create project
      Current_Project := Projects.Create_Empty_Project_For_File
        (Project_Filename, Graph_Filename, Checksum);

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
      Graph_Filename : String
        := Projects.Get_Bauhaus_IML_Graph_File (Filename);
   begin
      --  check this now to avoid loading the graph
      if (not Projects.Does_Project_Exist_File (Filename)) then
         raise Projects.Project_Does_Not_Exist_Exception;
      end if;

      --  create graph
      Logger.Info (-"Loading graph " & Graph_Filename);
      Giant.Graph_Lib.Load (Graph_Filename);

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
   --  Pins
   ---------------------------------------------------------------------------

   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String;
      Position    : in Vis.Logic.Vector_2d;
      Zoom_Level  : in Vis.Zoom_Level)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Vis_Windows.Add_Pin (Window, Name, Position, Zoom_Level);
      Gui_Manager.Add_Pin (Window_Name, Name);
   end Create_Pin;

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (Gui_Manager.Remove_Pin (Window_Name, Name)) then
         Vis_Windows.Remove_Pin (Window, Name);
         return True;
      else
         return False;
      end if;
   end Remove_Pin;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (New_Name /= Old_Name) then
         Vis_Windows.Change_Pin_Name (Window, Old_Name, New_Name);
         Gui_Manager.Rename_Pin (Window_Name, Old_Name, New_Name);
      end if;
   end Rename_Pin;

   procedure Show_Pin
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Set_Location_And_Zoom_Level
        (Vis_Windows.Get_Graph_Widget (Window),
         Vis_Windows.Get_Position (Window, Name),
         Vis_Windows.Get_Zoom (Window, Name));
   end Show_Pin;

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String)
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
      Lock : Graph_Widgets.Lock_Type;
   begin
      Vis_Windows.Add_Selection (Window, Selection);
      Graph_Widgets.Insert_Selection
        (Vis_Windows.Get_Graph_Widget (Window), Selection, Lock);
      Graph_Widgets.Release_Lock
        (Vis_Windows.Get_Graph_Widget (Window), Lock);
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

   function Exists_Selection
     (Window_Name : in String;
      Name        : in String)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Does_Selection_Exist (Window, Name);
   end Exists_Selection;

   function Get_Current_Selection
     (Window_Name : in String)
      return Graph_Lib.Selections.Selection
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Get_Selection
        (Window, Vis_Windows.Get_Current_Selection (Window));
   end Get_Current_Selection;

   function Get_Selection
     (Window_Name : in String;
      Name        : in String)
     return Graph_Lib.Selections.Selection
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Get_Selection (Window, Name);
   end Get_Selection;

--     function Get_Selection_Hightlight_ID
--       (Highlight_Status : in Vis_Windows.Selection_Highlight_Status)
--       return Config.Global_Data.Selection_High_Light_ID
--     is
--     begin
--        case (Vis_Windows.Color_1) is
--           when Vis_Windows.Selection_Highlight_Status =>
--              return Config.Global_Data.Color_1;
--        end case;
--     end Get_Selection_Hightlight_ID;

   procedure Hide_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (not Vis_Windows.Is_Faded_Out (Window, Name)) then
         Vis_Windows.Fade_Out_Selection (Window, Name);
      end if;
   end Hide_Selection;

   procedure Highlight_Selection
     (Window_Name      : in String;
      Name             : in String;
      Highlight_Status : in Vis_Windows.Selection_Highlight_Status)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Name);
--        Color : Config.Global_Data.Selection_High_Light_ID
--          := Get_Selection_Hightlight_ID (Highlight_Status);
   begin
      Vis_Windows.Set_Highlight_Status (Window, Name, Highlight_Status);
--        Graph_Widgets.Add_Local_Highlighting
--          (Projects.Get_Graph_Widget (Window), Selection, Color);
      Gui_Manager.Update_Selection (Window_Name, Name);
   end;

   procedure Insert_Selection
     (Window_Name           : in String;
      Selection_Name        : in String;
      Selection             : in Graph_Lib.Selections.Selection;
      Layout_Name           : in String;
      Position              : in Vis.Logic.Vector_2d            := Vis.Logic.Zero_2d;
      Additional_Parameters : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Lock : Graph_Widgets.Lock_Type;
   begin
      Vis_Windows.Add_Selection (Window, Selection);
      Graph_Widgets.Insert_Selection
        (Vis_Windows.Get_Graph_Widget (Window), Selection, Lock);
      Gui_Manager.Add_Selection (Window_Name, Selection_Name);
      if (Layout_Name /= "") then
         Apply_Layout (Window, Selection, Lock, Layout_Name, Position,
                       Additional_Parameters);
      end if;
   end Insert_Selection;

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Remove_Content       : in Boolean;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Name);
   begin
      if (Vis_Windows.Get_Standard_Selection (Window) = Name) then
         --  we need to raise this before Gui_Manager.Remove_Selection
         --  is called, otherwise we loose gui consistency
         raise Vis_Windows.Standard_Selection_May_Not_Be_Removed_Exception;
      end if;

      if (Gui_Manager.Remove_Selection (Window_Name, Name)) then
         --  FIX : unhighlight
         if (Remove_Content) then
            --  remove the selection from display before it is destroyed
            Graph_Widgets.Remove_Selection
              (Vis_Windows.Get_Graph_Widget (Window), Selection);
         end if;
         Vis_Windows.Remove_Selection (Window, Name);
         return True;
      end if;
      return False;
   end Remove_Selection;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (New_Name /= Old_Name) then
         Vis_Windows.Change_Selection_Name
           (Window, Old_Name, New_Name);
         Gui_Manager.Rename_Selection (Window_Name, Old_Name, New_Name);
      end if;
   end Rename_Selection;

   procedure Set_Current_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Previous_Selection_Name : String
        := Vis_Windows.Get_Current_Selection (Window);
   begin
      if (Name /= Previous_Selection_Name) then
         Unhighlight_Selection (Window_Name, Name);

         Vis_Windows.Set_Current_Selection (Window, Name);
         Gui_Manager.Update_Selection (Window_Name, Previous_Selection_Name);
         Gui_Manager.Update_Selection (Window_Name, Name);
      else
         Logger.Info (-"The selection is already active.");
      end if;
   end Set_Current_Selection;

   procedure Show_All_Selections
     (Window_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  iterate through all selections
      List := Vis_Windows.Get_All_Selections (Window);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         if (Vis_Windows.Is_Faded_Out
             (Window, Ada.Strings.Unbounded.To_String (Name))) then
            Vis_Windows.Fade_In_Selection
              (Window, Ada.Strings.Unbounded.To_String (Name));
         end if;
      end loop;
      String_Lists.Destroy (List);
   end;

   procedure Show_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (Vis_Windows.Is_Faded_Out (Window, Name)) then
         Vis_Windows.Fade_In_Selection (Window, Name);
      end if;
   end;

   procedure Unhighlight_Selection
     (Window_Name : in String;
      Name        : in String)
   is
   begin
      Highlight_Selection (Window_Name, Name, Vis_Windows.None);
   end Unhighlight_Selection;

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Create_Subgraph
     (Name : in String)
   is
      Subgraph : Graph_Lib.Subgraphs.Subgraph;
      Unique_Name : String := Get_Unique_Name (Name);
   begin
      Subgraph := Graph_Lib.Subgraphs.Create (Unique_Name);
      Graph_Lib.Subgraphs.Add_Node_Set (Subgraph, Graph_Lib.Get_All_Nodes);
      Graph_Lib.Subgraphs.Add_Edge_Set (Subgraph, Graph_Lib.Get_All_Edges);

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

   function Exists_Subgraph
     (Name : in String)
     return Boolean
   is
   begin
      return Projects.Does_Subgraph_Exist (Current_Project, Name);
   end Exists_Subgraph;

   function Get_Subgraph
     (Name        : in String)
     return Graph_Lib.Subgraphs.Subgraph
   is
   begin
      return Projects.Get_Subgraph (Current_Project, Name);
   end Get_Subgraph;

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
      if (New_Name /= Old_Name) then
         Projects.Change_Subgraph_Name
           (Current_Project, Old_Name, New_Name);
         Gui_Manager.Rename_Subgraph (Old_Name, New_Name);
      end if;
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
   --  Vis Styles
   ---------------------------------------------------------------------------

   procedure Set_Vis_Style
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Style : Config.Vis_Styles.Visualisation_Style_Access
        := Config.Vis_Styles.Initialize_Vis_Style_By_Name (Name);
   begin
      Logger.Debug ("setting vis style for " & Window_Name & ": "
                    & Name);

      Vis_Windows.Set_Vis_Style (Window, Name);
      Graph_Widgets.Set_Vis_Style (Vis_Windows.Get_Graph_Widget (Window),
                                   Style);
      Gui_Manager.Update_Vis_Style (Window_Name);
   end Set_Vis_Style;

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
      if (not Projects.Is_Vis_Window_Memory_Loaded
          (Current_Project, Name)) then
         --  the window is not even loaded
         return True;
      end if;

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
      -- FIX: remove vvv
      Create_Subgraph (Unique_Name & "_");
      Create_Selection_From_Subgraph (Subgraph_Name => Unique_Name & "_",
                                      Window_Name   => Unique_Name,
                                      Name          => Unique_Name);
      -- FIX: remove ^^^
      Open_Window (Unique_Name);
   end Create_Window;

   function Exists_Window
     (Name : in String)
     return Boolean
   is
   begin
      return Projects.Does_Vis_Window_Exist (Current_Project, Name);
   end Exists_Window;

   procedure Make_Room
     (Window_Name : in String;
      Center      : in Vis.Logic.Vector_2d;
      Width       : in Vis.Logic_Float;
      Height      : in Vis.Logic_Float)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Make_Room (Vis_Windows.Get_Graph_Widget (Window),
                               Center, Width, Height);
   end Make_Room;

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
      if (New_Name /= Old_Name) then
         Projects.Change_Vis_Window_Name
           (Current_Project, Old_Name, New_Name);
         Gui_Manager.Rename_Window (Old_Name, New_Name);
      end if;
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

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Set_Zoom_Level
     (Window_Name : in String;
      Zoom_Level  : in Vis.Zoom_Level)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Logger.Debug ("setting zoom level for " & Window_Name & ": "
                    & Vis.Zoom_Level'Image (Zoom_Level));
      --Graph_Widgets.Set_Zoom_Level ();
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Set_Zoom_Level;

end Giant.Controller;

