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
--  $RCSfile: giant-graph_widgets-states.ads,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/07/07 03:35:59 $
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


   ---------------------
   -- Drawing Ability --
   ---------------------

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class);

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class);


   -----------------
   -- Action Mode --
   -----------------

   procedure Enable_Action_Mode
     (Widget : access Graph_Widget_Record'Class);

   procedure Disable_Action_Mode
     (Widget : access Graph_Widget_Record'Class);


   -----------
   -- Locks --
   -----------

   procedure Create_New_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   :    out Lock_Type);

   procedure Destroy_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type);


   ------------------------
   -- Pollution tracking --
   ------------------------

   procedure Changed_Visual
     (Widget : access Graph_Widget_Record'Class);

   procedure Updated_Visual
     (Widget : access Graph_Widget_Record'Class);


   ---------------------
   -- State Inquiries --
   ---------------------

   function Is_Valid_Lock
     (Widget : access Graph_Widget_Record'Class;
      Lock   : in     Lock_Type)
     return Boolean;

   --  Returns True if any visible content on the graph widget might have
   --  changed and the visual buffer must be updated.
   function Has_Display_Changed
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

   --  Returns True if and only if 'Widget' is in action mode
   function Is_Action_Mode_Current
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

end Giant.Graph_Widgets.States;
