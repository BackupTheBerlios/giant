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
--  $RCSfile: giant-graph_widgets-drawing.ads,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/29 13:56:08 $
--
------------------------------------------------------------------------------
--
--  This package performs the actual drawing work for a graph widget.
--  It updates the buffers according to the modifications on the data
--  managed by the package 'Vis_Data' and then maps the buffers to the
--  'Drawable' provided by GtkAda.
--


package Giant.Graph_Widgets.Drawing is

   ----------------------------------------------------------------------------
   --  Settings must have been 'Set_Up' before.
   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class);

end Giant.Graph_Widgets.Drawing;
