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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets.adb,v $, $Revision: 1.3 $
--  $Author: keulsn $
--  $Date: 2003/06/23 23:37:17 $
--
------------------------------------------------------------------------------


with System;

with Gtk.Object;

with Giant.Graph_Widgets.Callbacks;
with Giant.Graph_Widgets.Drawing;
with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Widgets.Settings;

package body Giant.Graph_Widgets is

   -------------------------------
   -- Construction, Destruction --
   -------------------------------

   Class_Record : System.Address := System.Null_Address;

   ---------------------------------------------------------------------------
   --  Initializes the data structure. More initialization is done after the
   --  "realize" signal has been emitted on 'Widget'
   procedure Initialize
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      Gtk.Widget.Initialize_Widget (Widget);
      Gtk.Object.Initialize_Class_Record
        (Object       => Widget,
         Signals      => Notifications.Get_Signal_Array,
         Class_Record => Class_Record);
   end Initialize;

   procedure Create
     (Widget :    out Graph_Widget;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access
                       := Config.Vis_Styles.Get_Default_Vis_Style) is
   begin
      Widget := new Graph_Widget_Record;
      Initialize (Widget, Style);
   end Create;

   procedure Read_Graph_Widget
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Widget :    out Graph_Widget)  is
   begin
      raise Unimplemented;
   end Read_Graph_Widget;

   procedure Write_Graph_Widget
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Widget : access Graph_Widget_Record) is
   begin
      raise Unimplemented;
   end Write_Graph_Widget;


   --------------------------------------------
   -- Insertion, Deletion of Edges and Nodes --
   --------------------------------------------

   function Contains
     (Widget   : access Graph_Widget_Record'Class;
      Edge     : in     Graph_Lib.Edge_Id)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end Contains;

   function Contains
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end Contains;

   procedure Insert_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      raise Unimplemented;
   end Insert_Selection;

   procedure Insert_Selection_Difference
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      raise Unimplemented;
   end Insert_Selection_Difference;

   procedure Remove_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection) is
   begin
      raise Unimplemented;
   end Remove_Selection;

   procedure Clear
     (Widget : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end Clear;


   ----------------
   -- Crosshairs --
   ----------------

   procedure Set_Crosshair_Mode
     (Widget : access Graph_Widget_Record'Class;
      Enable : in     Boolean) is
   begin
      raise Unimplemented;
   end Set_Crosshair_Mode;


   ------------
   -- Layout --
   ------------

   procedure Lock_All_Content
     (Widget    : access Graph_Widget_Record'Class;
      Lock      :    out Lock_Type) is
   begin
      raise Unimplemented;
   end Lock_All_Content;

   procedure Lock_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      raise Unimplemented;
   end Lock_Selection;

   procedure Set_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id;
      Location  : in     Vis.Logic.Vector_2d;
      Lock      : in     Lock_Type) is
   begin
      raise Unimplemented;
   end Set_Top_Middle;
   pragma Inline (Set_Top_Middle);

   function Get_Top_Middle
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Vis.Logic.Vector_2d is
   begin
      raise Unimplemented;
      return Vis.Logic.Zero_2d;
   end Get_Top_Middle;
   pragma Inline (Get_Top_Middle);

   procedure Release_Lock
     (Widget    : access Graph_Widget_Record'Class;
      Lock      : in     Lock_Type) is
   begin
      raise Unimplemented;
   end Release_Lock;


   ---------------------------------------
   -- Layout manipulations without lock --
   ---------------------------------------

   procedure Make_Room
     (Widget    : access Graph_Widget_Record'Class;
      Center    : in     Vis.Logic.Vector_2d;
      Width     : in     Vis.Logic_Float;
      Height    : in     Vis.Logic_Float) is
   begin
      raise Unimplemented;
   end Make_Room;

   procedure Move_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : access Graph_Lib.Selections.Selection;
      Move      : in     Vis.Logic.Vector_2d) is
   begin
      raise Unimplemented;
   end Move_Selection;


   --------------------------
   -- Visualization Styles --
   --------------------------

   function Get_Vis_Style
     (Widget     : access Graph_Widget_Record'Class)
      return Config.Vis_Styles.Visualisation_Style_Access is
   begin
      raise Unimplemented;
      return Config.Vis_Styles.Get_Default_Vis_Style;
   end Get_Vis_Style;

   procedure Set_Vis_Style
     (Widget     : access Graph_Widget_Record'Class;
      Style      : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      raise Unimplemented;
   end Set_Vis_Style;


   ------------------
   -- Highlighting --
   ------------------

   procedure Add_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Selection_High_Light_ID) is
   begin
      raise Unimplemented;
   end Add_Local_Highlighting;

   procedure Remove_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Selection_High_Light_ID) is
   begin
      raise Unimplemented;
   end Remove_Local_Highlighting;

   procedure Add_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Subgraph_High_Light_ID) is
   begin
      raise Unimplemented;
   end Add_Global_Highlighting;

   procedure Remove_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Subgraph_High_Light_ID) is
   begin
      raise Unimplemented;
   end Remove_Global_Highlighting;

   procedure Clear_Highlighting
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end Clear_Highlighting;


   -----------------------
   -- Visual Attributes --
   -----------------------

   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end Is_Hidden;

   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Boolean is
   begin
      raise Unimplemented;
      return False;
   end Is_Hidden;

   procedure Set_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Hidden     : in     Boolean) is
   begin
      raise Unimplemented;
   end Set_Hidden;

   procedure Unhide_All
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end Unhide_All;

   procedure Change_Annotation_State
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id) is
   begin
      raise Unimplemented;
   end Change_Annotation_State;


   ------------------------
   -- Zooming and Moving --
   ------------------------

   function Get_Maximum_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level is
   begin
      raise Unimplemented;
      return 0.0;
   end Get_Maximum_Zoom_Level;

   function Get_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Zoom_Level is
   begin
      raise Unimplemented;
      return 0.0;
   end Get_Zoom_Level;

   procedure Set_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Zoom       : in     Vis.Zoom_Level) is
   begin
      raise Unimplemented;
   end Set_Zoom_Level;

   procedure Zoom_To_Edge
     (Widget     : access Graph_Widget_Record'Class;
      Edge       : in     Graph_Lib.Edge_Id) is
   begin
      raise Unimplemented;
   end Zoom_To_Edge;

   procedure Zoom_To_Selection
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection) is
   begin
      raise Unimplemented;
   end Zoom_To_Selection;

   procedure Zoom_To_All
     (Widget     : access Graph_Widget_Record'Class) is
   begin
      raise Unimplemented;
   end Zoom_To_All;

   function Get_Location
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Vector_2d is
   begin
      raise Unimplemented;
      return Vis.Logic.Zero_2d;
   end Get_Location;

   procedure Set_Location
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d) is
   begin
      raise Unimplemented;
   end Set_Location;

   procedure Set_Location_And_Zoom_Level
     (Widget     : access Graph_Widget_Record'Class;
      Location   : in     Vis.Logic.Vector_2d;
      Zoom       : in     Vis.Zoom_Level) is
   begin
      raise Unimplemented;
   end Set_Location_And_Zoom_Level;

end Giant.Graph_Widgets;
