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
--  $RCSfile: giant-layouts-matrix_layouts.ads,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/06/10 00:30:41 $
--
------------------------------------------------------------------------------
--
--  Contains the treelayout-algorithm
--

with Giant.Evolutions;
with Giant.Graph_Lib.Selections;

package Layouts.Matrix_Layouts is

   type Matrix_Layout is
     new Giant.Evolutions.Concurrent_Evolution with private;
   type Matrix_Layout_Access is access Matrix_Layout;

   ---------------------------------------------------------------------------
   --  Initializes the tree-layout-algorithm
   --
   --  The evolutions.Initialize is called in Layouts
   --
   --  Parameters (cp. specification 11.2.2: Matrixlayout / Parameter):
   --    Selection_To_Layout         : The selection containing the nodes and
   --                                  edges to layout
   --    Widget_Containing_Selection : Graph_Widet where the nodes to layout
   --                                  reside
   --    Addtional_Parameters        : <Target_Position>
   --      Target_Position           : Position on window, where the
   --                                  upper-left corner of the matrix has to
   --                                  be placed
   --    Additional_Parameters_Error' :
   --    Result'                      :
   procedure Initialize
     (Selection_To_Layout         :
        in     Giant.Graph_Lib.Selections.Selection;
      Addtional_Parameters        : in     String;
      Additional_Parameters_Error :    out String;
      Result                      :    out Create_Result
     );

end Layouts.Matrix_Layouts;
