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
--  $RCSfile: giant-graph_widgets-states.ads,v $, $Revision: 1.7 $
--  $Author: keulsn $
--  $Date: 2003/07/20 23:20:04 $
--
------------------------------------------------------------------------------
--
--  This package implements an finite automaton tracking all the logical
--  states a graph widget can be in. Queries to the functions in this
--  package allow to decide if an action can be performed on a graph widget.
--
--  This package also updates the mouse cursor shown on a graph widget.
--


package Giant.Graph_Widgets.States is

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class);

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class);

   procedure Realized
     (Widget : access Graph_Widget_Record'Class);


   ---------------------
   -- Drawing Ability --
   ---------------------

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class);

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class);


   -----------
   -- Areas --
   -----------

   procedure Logic_Area_Changed
     (Widget : access Graph_Widget_Record'Class);

   procedure Logic_Area_Updated
     (Widget : access Graph_Widget_Record'Class);

   function Must_Update_Logic_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   procedure Visual_Area_Changed
     (Widget : access Graph_Widget_Record'Class);

   procedure Visual_Area_Updated
     (Widget : access Graph_Widget_Record'Class);

   function Must_Update_Visual_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   -----------------
   -- Action Mode --
   -----------------

   procedure Enable_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   procedure Disable_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   --  Returns True if and only if 'Widget' is in action mode
   function Is_Action_Mode_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   -----------
   -- Locks --
   -----------

   procedure Create_New_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   :    out Lock_Type);

   procedure Destroy_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type);

   function Is_Locked
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   function Is_Valid_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type)
     return Boolean;

   --  Returns True if the last lock was released, but no flush operation
   --  has happened yet.
   function Must_Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  Must be called when all locked modifications are being applied.
   --  'Must_Flush_Locked_Content' will return False after.
   procedure Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class);


   ------------------------
   -- Pollution tracking --
   ------------------------

   procedure Changed_Visual
     (Widget : access Graph_Widget_Record'Class);

   procedure Updated_Visual
     (Widget : access Graph_Widget_Record'Class);

   procedure Changed_Temporary
     (Widget : access Graph_Widget_Record'Class);

   procedure Updated_Temporary
     (Widget : access Graph_Widget_Record'Class);


   -------------
   -- Cursors --
   -------------

   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);

   procedure Set_Action_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor);


   --------------------
   -- Mouse Handling --
   --------------------

   procedure Begin_Click_On_Background
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d);

   procedure Begin_Click_On_Edge
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d;
      Edge   : in     Vis_Data.Vis_Edge_Id);

   procedure Begin_Click_On_Node
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d;
      Node   : in     Vis_Data.Vis_Node_Id);

   procedure End_Click
     (Widget : access Graph_Widget_Record'Class);

   function Get_Click_Edge
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Data.Vis_Edge_Id;

   function Get_Click_Node
     (Widget : access Graph_Widget_Record'Class)
     return Vis_Data.Vis_Node_Id;

   function Get_Click_Point
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d;

   function Is_Click_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  To be used during click, drag and rectangle mode to keep track of the
   --  position of the mouse cursor.
   procedure Set_Mouse_Position
     (Widget : access Graph_Widget_Record'Class;
      Point  : in     Vis.Absolute.Vector_2d);

   function Get_Mouse_Move_Distance
     (Widget : access Graph_Widget_Record'Class)
     return Vis.Absolute.Vector_2d;


   ----------------------------------------------------------------------------
   --  Starts a rectangle after a click has started.
   procedure Begin_Rectangle
     (Widget : access Graph_Widget_Record'Class);

   procedure End_Rectangle
     (Widget : access Graph_Widget_Record'Class);

   function Is_Rectangle_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   ----------------------------------------------------------------------------
   --  Starts a drag after a click has started.
   procedure Begin_Drag
     (Widget : access Graph_Widget_Record'Class);

   procedure End_Drag
     (Widget : access Graph_Widget_Record'Class);

   function Is_Drag_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


   ---------------------
   -- State Inquiries --
   ---------------------

   --  Returns True if the visual buffer must be updated
   --  (see 'Has_Display_Changed') or if any floating object has
   --  changed and thus a "draw" signal must be emitted on 'Widget'
   function Must_Queue_Draw
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  Returns True if any visible content on the graph widget might have
   --  changed and the visual buffer must be updated. If only floating
   --  objects have changed, then the graph widget must be redrawn, but
   --  this function might return False anyway! For most cases
   --  'Must_Queue_Draw' is what you want.
   function Has_Display_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  Returns True if any floating objects on the graph widget might have
   --  changed and the display needs to be redrawn. If only non-floating
   --  objects have changed, then the graph widget must be redrawn, but
   --  this function might return False anyway! For most cases
   --  'Must_Queue_Draw' is what you want.
   function Has_Temporary_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  Returns True if the widget might resize:
   --  * check the size of the widget
   --  * allocate and destroy pixmaps
   function Can_Resize
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;


private

   function Get_Default_Default_Cursor
     return Gdk.Cursor.Gdk_Cursor;

   function Get_Default_Waiting_Cursor
     return Gdk.Cursor.Gdk_Cursor;

   function Get_Default_Action_Cursor
     return Gdk.Cursor.Gdk_Cursor;

   procedure Update_Cursor
     (Widget    : access Graph_Widget_Record'Class;
      Force_Set : in     Boolean := False);

end Giant.Graph_Widgets.States;
