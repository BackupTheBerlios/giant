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
--  $RCSfile: giant-graph_widgets-states.adb,v $, $Revision: 1.4 $
--  $Author: keulsn $
--  $Date: 2003/07/08 19:41:48 $
--
------------------------------------------------------------------------------


package body Giant.Graph_Widgets.States is

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class) is
   begin
      null;
   end Set_Up;

   procedure Shut_Down
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Lock_Sets.Destroy (Widget.States.Locks);
   end Shut_Down;


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
   end Enable_Action_Mode;

   procedure Disable_Action_Mode
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Widget.States.Action_Mode := False;
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


   ---------------------
   -- State Inquiries --
   ---------------------

   function Has_Display_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      return Widget.States.Visual_Polluted;
   end Has_Display_Changed;

end Giant.Graph_Widgets.States;
