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
--  $RCSfile: giant-graph_widgets-states.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/30 02:55:18 $
--
------------------------------------------------------------------------------
--
--  This package implements an finite automaton tracking all the logical
--  states a graph widget can be in. Queries to the functions in this
--  package allow to decide if an action can be performed on a graph widget.
--


package Giant.Graph_Widgets.States is

   procedure Enable_Drawing
     (Widget : access Graph_Widget_Record'Class);

   procedure Disable_Drawing
     (Widget : access Graph_Widget_Record'Class);

   function Is_Visible
     (Widget : access Graph_Widget_Record'Class)
     return Boolean;

end Giant.Graph_Widgets.States;
