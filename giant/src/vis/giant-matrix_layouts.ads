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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-matrix_layouts.ads,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/07/02 11:59:43 $
--
------------------------------------------------------------------------------
--
--  Contains the treelayout-algorithm
--

with Giant.Evolutions;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Matrix_Layouts is

   type Matrix_Layout_Record is
     new Evolutions.Concurrent_Evolution with private;

   type Matrix_Layout is access all Matrix_Layout_Record'Class;

   ---------------------
   --  Initilization  --
   ---------------------

   ---------------------------------------------------------------------------
   --  Initializes the tree-layout-algorithm
   --
   --  Parameters (cp. specification 11.2.2: Matrixlayout / Parameter):
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes to layout
   --      Edges are ignored
   --
   --    Widget:
   --      Graph_Widet where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock of the widget, released at the end of the layout
   --
   --    Target_Position:
   --      Position on window, where the upper-left corner of the matrix has to
   --      be placed
   --
   --  Returns:
   --    derived Evolutions-Object to do the layout
   --
   function Initialize
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d)
     return Matrix_Layout;

   -------------------
   --  Calculation  --
   -------------------

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Matrix_Layout_Record;
      Canceled : in     Boolean);

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Matrix_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action);

private
   type Layout_State is (Init, Calc);

   type Matrix_Layout_Record is
     new Evolutions.Concurrent_Evolution with record
        Widget          : Giant.Graph_Widgets.Graph_Widget;
        Widget_Lock     : Giant.Graph_Widgets.Lock_Type;
        Nodes_To_Layout : Giant.Graph_Lib.Node_Id_Set;
        Target_Position : Giant.Vis.Logic.Vector_2d;
        State           : Layout_State;
        Width           : Natural;
     end record;

end Giant.Matrix_Layouts;
