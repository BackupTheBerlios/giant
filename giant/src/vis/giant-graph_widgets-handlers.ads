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
--  $RCSfile: giant-graph_widgets-handlers.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--
--  This Package provides the 'To_Marshaller' functions necessary to connect
--  to the signals emitted by 'Graph_Widgets.Graph_Widget'.
--  It also provides instances of the generic package
--  Gtk.Handlers.<Callback_type>. Use the 'Connect' subprograms provided in
--  these packages.
--


with Gtk.Handlers;

package Giant.Graph_Widgets.Handlers is

   ----------------------------------
   -- Signals without return value --
   ----------------------------------

   package No_Return is new Gtk.Handlers.Callback
     (Widget_Type => Graph_Widget_Record);

   ----------------------------------------------------------------------------
   --  Emitted whenever the visible logical area changes. This change can
   --  be due to resizing of the widget, moving over the logical area,
   --  changing the zoom level
   Visible_Area_Changed_Signal : constant String := "visible_area_changed";

   ----------------------------------------------------------------------------
   --  Handler for signal Visible_Area_Changed_Signal
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The visible area inside 'Widget'
   type Visible_Area_Changed_Signal_Cb is access procedure
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);

   ----------------------------------------------------------------------------
   --  Visible_Area_Changed
   function To_Marshaller
     (Cb : in Visible_Area_Changed_Signal_Cb)
     return No_Return.Marshallers.Marshaller;

end Giant.Graph_Widgets.Handlers;
