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
--  $RCSfile: giant-graph_widgets-states.adb,v $, $Revision: 1.8 $
--  $Author: keulsn $
--  $Date: 2003/07/14 23:12:18 $
--
------------------------------------------------------------------------------


with Gdk.Types;
with Gdk.Window;

package body Giant.Graph_Widgets.States is


   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Cursors :=
        (Default => Get_Default_Default_Cursor,
         Waiting => Get_Default_Waiting_Cursor,
         Action  => Get_Default_Action_Cursor);
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Lock_Sets.Destroy (Widget.States.Locks);
   end Shut_Down;

   procedure Realized
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Realized;


   -----------
   -- Areas --
   -----------

   procedure Logic_Area_Changed
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Logic_Area_Changed := True;
   end Logic_Area_Changed;

   procedure Logic_Area_Updated
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Logic_Area_Changed := False;
   end Logic_Area_Updated;

   function Must_Update_Logic_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Logic_Area_Changed and then
        not Is_Locked (Widget);
   end Must_Update_Logic_Area;


   procedure Visual_Area_Changed
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Area_Changed := True;
   end Visual_Area_Changed;

   procedure Visual_Area_Updated
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Area_Changed := False;
   end Visual_Area_Updated;

   function Must_Update_Visual_Area
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Visual_Area_Changed and then
        not Is_Locked (Widget);
   end Must_Update_Visual_Area;


   ---------------------
   -- Drawing Ability --
   ---------------------

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Drawing_Ready := True;
   end Enable_Drawing;

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Drawing_Ready := False;
   end Disable_Drawing;


   -----------------
   -- Action Mode --
   -----------------

   procedure Enable_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Action_Mode := True;
      Update_Cursor (Widget);
   end Enable_Action_Mode;

   procedure Disable_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Action_Mode := False;
      Update_Cursor (Widget);
   end Disable_Action_Mode;

   function Is_Action_Mode_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Action_Mode;
   end Is_Action_Mode_Current;


   -----------
   -- Locks --
   -----------

   procedure Create_New_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   :    out Lock_Type) is
   begin
      Widget.States.Highest_Lock := Widget.States.Highest_Lock + 1;
      Lock := Widget.States.Highest_Lock;
      Lock_Sets.Insert
        (A_Set   => Widget.States.Locks,
         Element => Lock);
      Update_Cursor (Widget);
   end Create_New_Lock;

   procedure Destroy_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type) is

      Lock_Removed : Boolean;
   begin
      Lock_Sets.Remove_If_Exists
        (A_Set   => Widget.States.Locks,
         Element => Lock,
         Found   => Lock_Removed);
      if Lock_Sets.Is_Empty (Widget.States.Locks) then
         Widget.States.Highest_Lock := 0;
         if Lock_Removed then
            Widget.States.Lock_Flush_Pending := True;
         end if;
      end if;
   end Destroy_Lock;

   function Is_Locked
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return not Lock_Sets.Is_Empty (Widget.States.Locks);
   end Is_Locked;

   function Is_Valid_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type)
     return Boolean is
   begin
      return Lock_Sets.Is_Member (Widget.States.Locks, Lock);
   end Is_Valid_Lock;

   function Must_Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Lock_Flush_Pending and then
        Gtk.Widget.Realized_Is_Set (Widget);
   end Must_Flush_Locked_Content;

   procedure Flush_Locked_Content
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Lock_Flush_Pending := False;
      Update_Cursor (Widget);
   end Flush_Locked_Content;


   -------------------------
   --  Pollution tracking --
   -------------------------

   procedure Changed_Visual
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Polluted := True;
   end Changed_Visual;

   procedure Updated_Visual
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Visual_Polluted := False;
   end Updated_Visual;


   -------------
   -- Cursors --
   -------------

   procedure Set_Default_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Default) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Default_Cursor;

   procedure Set_Waiting_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Waiting) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Waiting_Cursor;

   procedure Set_Action_Cursor
     (Widget : access Graph_Widget_Record'Class;
      Cursor : in     Gdk.Cursor.Gdk_Cursor) is
   begin
      Widget.States.Cursors (Action) := Cursor;
      Update_Cursor (Widget => Widget, Force_Set => True);
   end Set_Action_Cursor;


   procedure Update_Cursor
     (Widget    : access Graph_Widget_Record'Class;
      Force_Set : in     Boolean := False) is

      New_State : Cursor_State_Type;
   begin
      --  can set cursor only if there is a Gdk_Window
      if Gtk.Widget.Realized_Is_Set (Widget) then
         if Widget.States.Action_Mode then
            New_State := Action;
         elsif not Lock_Sets.Is_Empty (Widget.States.Locks) then
            New_State := Waiting;
         else
            New_State := Default;
         end if;

         if Force_Set or else New_State /= Widget.States.Current_Cursor then
            Widget.States.Current_Cursor := New_State;
            Gdk.Window.Set_Cursor
              (Get_Window (Widget),
               Widget.States.Cursors (Widget.States.Current_Cursor));
         end if;
      end if;
   end Update_Cursor;


   ---------------------
   -- State Inquiries --
   ---------------------

   function Has_Display_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Visual_Polluted;
   end Has_Display_Changed;

   function Can_Resize
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Drawing_Ready and then
        Gtk.Widget.Realized_Is_Set (Widget);
   end Can_Resize;


   Default_Default_Cursor : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
   Default_Waiting_Cursor : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
   Default_Action_Cursor  : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;

   function Get_Default_Default_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Waiting_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Waiting_Cursor,
            Cursor_Type => Gdk.Types.Left_Ptr);
      end if;
      return Default_Waiting_Cursor;
   end Get_Default_Default_Cursor;

   function Get_Default_Waiting_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Action_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Action_Cursor,
            Cursor_Type => Gdk.Types.Watch);
      end if;
      return Default_Waiting_Cursor;
   end Get_Default_Waiting_Cursor;

   function Get_Default_Action_Cursor
     return Gdk.Cursor.Gdk_Cursor is
   begin
      if Gdk.Cursor."=" (Default_Action_Cursor, Gdk.Cursor.Null_Cursor) then
         Gdk.Cursor.Gdk_New
           (Widget      => Default_Action_Cursor,
            Cursor_Type => Gdk.Types.Crosshair);
      end if;
      return Default_Action_Cursor;
   end Get_Default_Action_Cursor;

end Giant.Graph_Widgets.States;
