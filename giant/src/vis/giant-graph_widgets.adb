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
--  $RCSfile: giant-graph_widgets.adb,v $, $Revision: 1.13 $
--  $Author: keulsn $
--  $Date: 2003/07/07 18:39:23 $
--
------------------------------------------------------------------------------


with System;

with Gtk.Object;

with Giant.Graph_Widgets.Callbacks;
with Giant.Graph_Widgets.Drawing;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Widgets.Positioning;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;

package body Giant.Graph_Widgets is

   function "="
     (Left  : Vis_Data.Vis_Edge_Id;
      Right : Vis_Data.Vis_Edge_Id)
     return Boolean renames Vis_Data."=";

   function "="
     (Left  : Vis_Data.Vis_Node_Id;
      Right : Vis_Data.Vis_Node_Id)
     return Boolean renames Vis_Data."=";


   package Graph_Widget_Logger is new Logger
     (Name => "Giant.Graph_Widgets");


   -------------------------------
   -- Construction, Destruction --
   -------------------------------

   Class_Record : System.Address := System.Null_Address;

   ---------------------------------------------------------------------------
   --  Initializes the data structure. More initialization is done after the
   --  "realize" signal has been emitted on 'Widget'
   procedure Initialize
     (Widget       : access Graph_Widget_Record'Class;
      Style        : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      Gtk.Widget.Initialize_Widget (Widget);
      Gtk.Object.Initialize_Class_Record
        (Object                    => Widget,
         Signals                   => Handlers.Get_Signal_Array,
         Class_Record              => Class_Record,
         Parameters                => Handlers.Get_Signal_Parameters,
         Scroll_Adjustments_Signal => Handlers.Get_Scroll_Adjustments_Signal);

      Vis_Data.Set_Up (Widget.Manager);
      Widget.Edge_Map := Edge_Id_Mappings.Create;
      Vis_Data.Reset_Pool (Widget.Edge_Layers);
      Widget.Node_Map := Node_Id_Mappings.Create;
      Vis_Data.Reset_Pool (Widget.Node_Layers);

      Callbacks.Connect_All_Callbacks (Widget);

      States.Set_Up (Widget);
      Positioning.Set_Up (Widget, Default_Zoom_Level);
      --  Cannot set up yet, but must set visualization style.
      Settings.Set_Style (Widget, Style);
   end Initialize;

   procedure Create
     (Widget       :    out Graph_Widget;
      Style        : in     Config.Vis_Styles.Visualisation_Style_Access :=
        Config.Vis_Styles.Get_Default_Vis_Style) is
   begin
      Widget := new Graph_Widget_Record;
      Initialize (Widget, Style);
   end Create;

   procedure Shut_Down_Graph_Widget
     (Widget : access Graph_Widget_Record'Class) is

      Edges : Edge_Id_Mappings.Values_Iter;
      Edge  : Vis_Data.Vis_Edge_Id;
      Nodes : Node_Id_Mappings.Values_Iter;
      Node  : Vis_Data.Vis_Node_Id;
   begin
      Vis_Data.Destroy (Widget.Manager);
      Positioning.Shut_Down (Widget);
      States.Shut_Down (Widget);

      Edges := Edge_Id_Mappings.Make_Values_Iter (Widget.Edge_Map);
      while Edge_Id_Mappings.More (Edges) loop
         Edge_Id_Mappings.Next (Edges, Edge);
         Vis_Data.Destroy (Edge);
      end loop;

      Nodes := Node_Id_Mappings.Make_Values_Iter (Widget.Node_Map);
      while Node_Id_Mappings.More (Nodes) loop
         Node_Id_Mappings.Next (Nodes, Node);
         Vis_Data.Destroy (Node);
      end loop;
   end Shut_Down_Graph_Widget;

   procedure Read_Graph_Widget
     (Stream : in     Bauhaus_IO.In_Stream_Type;
      Widget :    out Graph_Widget;
      Pool   : in     Node_Annotations.Node_Annotation_Access)  is
   begin
      raise Unimplemented;
   end Read_Graph_Widget;

   procedure Write_Graph_Widget
     (Stream : in     Bauhaus_IO.Out_Stream_Type;
      Widget : access Graph_Widget_Record) is
   begin
      raise Unimplemented;
   end Write_Graph_Widget;


   -------------------
   -- Configuration --
   -------------------

   procedure Set_Node_Annotations
     (Widget : access Graph_Widget_Record'Class;
      Pool   : in     Node_Annotations.Node_Annotation_Access) is
   begin
      Settings.Set_Annotation_Pool (Widget, Pool);
      --  Update on all nodes!
      raise Unimplemented;
   end Set_Node_Annotations;

   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      raise Unimplemented;
   end Set_Default_Cursor;

   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      raise Unimplemented;
   end Set_Waiting_Cursor;


   --------------------------------------------
   -- Insertion, Deletion of Edges and Nodes --
   --------------------------------------------

   function Contains
     (Widget   : access Graph_Widget_Record'Class;
      Edge     : in     Graph_Lib.Edge_Id)
     return Boolean is
   begin
      return Edge_Id_Mappings.Is_Bound (Widget.Edge_Map, Edge);
   end Contains;

   function Contains
     (Widget    : access Graph_Widget_Record'Class;
      Node      : in     Graph_Lib.Node_Id)
     return Boolean is
   begin
      return Node_Id_Mappings.Is_Bound (Widget.Node_Map, Node);
   end Contains;

   procedure Insert_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      States.Create_New_Lock (Widget, Lock);
      States.Changed_Visual (Widget);

   end Insert_Selection;

   procedure Insert_Selection_Difference
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in out Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is

      generic
         type Object_Type is private;
         with function "<"
           (Left  : in Object_Type;
            Right : in Object_Type)
           return Boolean;
         with function "="
           (Left  : in Object_Type;
            Right : in Object_Type)
           return Boolean;
         with package Object_Sets is new Ordered_Sets
           (Item_Type => Object_Type,
            "<"       => "<",
            "="       => "=");
         with function Is_Known
           (Widget : access Graph_Widget_Record'Class;
            Object : Object_Type)
           return Boolean;
         with procedure Remove_Set
           (Selection : in out Graph_Lib.Selections.Selection;
            Set       : in     Object_Sets.Set);
      procedure Remove_Known
        (Selection : in out Graph_Lib.Selections.Selection;
         Set       : in     Object_Sets.Set);

      procedure Remove_Known
        (Selection : in out Graph_Lib.Selections.Selection;
         Set       : in     Object_Sets.Set) is

         Known    : Object_Sets.Set      := Object_Sets.Empty_Set;
         Iterator : Object_Sets.Iterator := Object_Sets.Make_Iterator (Set);
         Current  : Object_Type;
      begin
         while Object_Sets.More (Iterator) loop
            Object_Sets.Next (Iterator, Current);
            if Is_Known (Widget, Current) then
               Object_Sets.Insert
                 (A_Set   => Known,
                  Element => Current);
            end if;
         end loop;
         Object_Sets.Destroy (Iterator);
         Remove_Set (Selection, Set);
         Object_Sets.Destroy (Known);
      end Remove_Known;

      procedure Remove_Edges is new Remove_Known
        (Object_Type  => Graph_Lib.Edge_Id,
         "="          => Graph_Lib."=",
         "<"          => Graph_Lib."<",
         Object_Sets  => Graph_Lib.Edge_Id_Sets,
         Is_Known     => Contains,
         Remove_Set   => Graph_Lib.Selections.Remove_Edge_Set);
      procedure Remove_Nodes is new Remove_Known
        (Object_Type  => Graph_Lib.Node_Id,
         "="          => Graph_Lib."=",
         "<"          => Graph_Lib."<",
         Object_Sets  => Graph_Lib.Node_Id_Sets,
         Is_Known     => Contains,
         Remove_Set   => Graph_Lib.Selections.Remove_Node_Set);

   begin
      Remove_Edges (Selection, Graph_Lib.Selections.Get_All_Edges (Selection));
      Remove_Nodes (Selection, Graph_Lib.Selections.Get_All_Nodes (Selection));
      Insert_Selection (Widget, Selection, Lock);
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


   -----------------
   -- Action Mode --
   -----------------

   procedure Start_Action_Mode
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      States.Enable_Action_Mode (Widget);
   end Start_Action_Mode;

   procedure Cancel_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.Disable_Action_Mode (Widget);
   end Cancel_Action_Mode;

   function Is_Action_Mode_Active
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return States.Is_Action_Mode_Current (Widget);
   end Is_Action_Mode_Active;


   ------------
   -- Layout --
   ------------

   procedure Lock_All_Content
     (Widget    : access Graph_Widget_Record'Class;
      Lock      :    out Lock_Type) is
   begin
      States.Create_New_Lock (Widget, Lock);
   end Lock_All_Content;

   procedure Lock_Selection
     (Widget    : access Graph_Widget_Record'Class;
      Selection : in     Graph_Lib.Selections.Selection;
      Lock      :    out Lock_Type) is
   begin
      --  Locking only one special set of edges and nodes is not yet possible.
      States.Create_New_Lock (Widget, Lock);
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

   function Get_Current_Maximum_Node_Width
     (Widget    : access Graph_Widget_Record'Class)
     return Vis.Logic_Float is
   begin
      raise Unimplemented;
      return 0.0;
   end Get_Current_Maximum_Node_Width;

   function Get_Current_Node_Width
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float is
   begin
      raise Unimplemented;
      return 0.0;
   end Get_Current_Node_Width;

   function Get_Current_Node_Height
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
     return Vis.Logic_Float is
   begin
      raise Unimplemented;
      return 0.0;
   end Get_Current_Node_Height;

   procedure Release_Lock
     (Widget    : access Graph_Widget_Record'Class;
      Lock      : in     Lock_Type) is
   begin
      States.Destroy_Lock (Widget, Lock);
--        if ... then
--        end if;
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
      return Settings.Get_Style (Widget);
   end Get_Vis_Style;

   procedure Set_Vis_Style
     (Widget     : access Graph_Widget_Record'Class;
      Style      : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      Settings.Set_Style (Widget, Style);
      raise Unimplemented;
      --  General redraw
   end Set_Vis_Style;


   ------------------
   -- Highlighting --
   ------------------

   procedure Add_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Global_Data.Selection_High_Light_ID) is
   begin
      raise Unimplemented;
   end Add_Local_Highlighting;

   procedure Remove_Local_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Selection  : access Graph_Lib.Selections.Selection;
      Color      : in     Config.Global_Data.Selection_High_Light_ID) is
   begin
      raise Unimplemented;
   end Remove_Local_Highlighting;

   procedure Add_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Global_Data.Subgraph_High_Light_ID) is
   begin
      raise Unimplemented;
   end Add_Global_Highlighting;

   procedure Remove_Global_Highlighting
     (Widget     : access Graph_Widget_Record'Class;
      Subgraph   : access Graph_Lib.Subgraphs.Subgraph;
      Color      : in     Config.Global_Data.Subgraph_High_Light_ID) is
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

      Vis_Edge : Vis_Data.Vis_Edge_Id := Look_Up (Widget, Edge);
   begin
      if Vis_Edge = null then
         raise Unknown_Edge_Id;
      end if;
      return Vis_Data.Is_Hidden (Vis_Edge);
   end Is_Hidden;

   function Is_Hidden
     (Widget     : access Graph_Widget_Record'Class;
      Node       : in     Graph_Lib.Node_Id)
     return Boolean is

      Vis_Node : Vis_Data.Vis_Node_Id := Look_Up (Widget, Node);
   begin
      if Vis_Node = null then
         raise Unknown_Node_Id;
      end if;
      return Vis_Data.Is_Hidden (Vis_Node);
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

   function Get_Logical_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d is
   begin
      return Vis.Logic.Combine_Rectangle (0.0, 0.0, 500.0, 200.0);
   end Get_Logical_Area;

   function Get_Visible_Area
     (Widget     : access Graph_Widget_Record'Class)
     return Vis.Logic.Rectangle_2d is
   begin
      return Vis.Logic.Combine_Rectangle (10.0, -3.0, 50.0, 17.0);
   end Get_Visible_Area;

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


   -------------
   -- Helpers --
   -------------

   procedure Resize_Graph_Widget
     (Widget : access Graph_Widget_Record'Class;
      Size   : in     Vis.Absolute.Vector_2d) is
   begin
      ---------------------------------------------raise Unimplemented;
      null;
   end Resize_Graph_Widget;

   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id;
      Edge       :    out Vis_Data.Vis_Edge_Id) is

      Source      : Vis_Data.Vis_Node_Id;
      Target      : Vis_Data.Vis_Node_Id;
      Inflections : Natural;
   begin
      Edge := Look_Up (Widget, Graph_Edge);
      if Edge = null then
         Source := Look_Up (Widget, Graph_Lib.Get_Source_Node (Graph_Edge));
         Target := Look_Up (Widget, Graph_Lib.Get_Target_Node (Graph_Edge));
         if Source /= null and then Target /= null then
            if Source = Target then
               Inflections := 4;
            else
               Inflections := 0;
            end if;
            Vis_Data.Enlarge_Pool (Widget.Edge_Layers);
            Edge := Vis_Data.Create_Edge
              (Graph_Edge  => Graph_Edge,
               Source      => Source,
               Target      => Target,
               Layer       => Vis_Data.Get_Highest_Layer (Widget.Edge_Layers),
               Inflections => Inflections);
            Edge_Id_Mappings.Bind
              (Map         => Widget.Edge_Map,
               Key         => Graph_Edge,
               Value       => Edge);
         end if;
      end if;
   end Find_Or_Create;

   procedure Find_Or_Create
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id;
      Node       :    out Vis_Data.Vis_Node_Id) is
   begin
      Node := Look_Up (Widget, Graph_Node);
      if Node = null then
         Vis_Data.Enlarge_Pool (Widget.Node_Layers);
         Node := Vis_Data.Create_Node
           (Graph_Node  => Graph_Node,
            Layer       => Vis_Data.Get_Highest_Layer (Widget.Node_Layers));
         Vis_Data.Set_Annotated
           (Node        => Node,
            State       => Settings.Has_Annotation (Widget, Node));
         Node_Id_Mappings.Bind
           (Map         => Widget.Node_Map,
            Key         => Graph_Node,
            Value       => Node);
      end if;
   end Find_Or_Create;

   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Edge : in     Graph_Lib.Edge_Id)
     return Vis_Data.Vis_Edge_Id is
   begin
      return Edge_Id_Mappings.Fetch (Widget.Edge_Map, Graph_Edge);
   exception
      when Edge_Id_Mappings.Not_Bound =>
         return null;
   end Look_Up;

   function Look_Up
     (Widget     : access Graph_Widget_Record'Class;
      Graph_Node : in     Graph_Lib.Node_Id)
     return Vis_Data.Vis_Node_Id is
   begin
      return Node_Id_Mappings.Fetch (Widget.Node_Map, Graph_Node);
   exception
      when Node_Id_Mappings.Not_Bound =>
         return null;
   end Look_Up;

end Giant.Graph_Widgets;
