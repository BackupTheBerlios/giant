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
--  $RCSfile: giant-graph_widgets-states.ads,v $, $Revision: 1.6 $
--  $Author: keulsn $
--  $Date: 2003/07/12 03:33:56 $
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


   ---------------------
   -- State Inquiries --
   ---------------------

   --  Returns True if any visible content on the graph widget might have
   --  changed and the visual buffer must be updated.
   function Has_Display_Changed
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
