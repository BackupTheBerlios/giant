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
--  $RCSfile: giant-graph_widgets-states.adb,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/07/02 16:49:15 $
--
------------------------------------------------------------------------------


package body Giant.Graph_Widgets.States is

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      ----------------------------raise Unimplemented;
      null;
   end Enable_Drawing;

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class) is
   begin
      ----------------------------raise Unimplemented;
      null;
   end Disable_Drawing;

   function Is_Visible
     (Widget : access Graph_Widget_Record'Class)
     return Boolean is
   begin
      ----------------------------raise Unimplemented;
      return False;
   end Is_Visible;

end Giant.Graph_Widgets.States;
