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
--  $RCSfile: giant-layout_factory.ads,v $, $Revision: 1.5 $
--  $Author: koppor $
--  $Date: 2003/07/03 01:12:17 $
--
------------------------------------------------------------------------------
--
--  Used as interface between the controller and the layout-algorithms
--  This package is used to offer the parameter-parsing and the calling
--    of the layout-algorithms
--

with Giant.Evolutions;
with Giant.Graph_Widgets;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;

package Giant.Layout_Factory is

   Additonal_Parameters_Error : exception;
   Invalid_Format             : exception;
   Other_Error                : exception;
   Unknown_Algorithm          : exception;

   ----------------------------------------------------------------------------
   --  Initialises an layout-algorithm
   --
   --  If an error occurs, an exception is risen
   --    if Additional_Parameters_Error is risen, Additional
   --
   --
   --  The resulting object has to be destroyed after usage
   --
   --  Parameters:
   --
   --    Algorithm:
   --      String, which describes the algorithm to be used
   --
   --    Selection_To_Layout:
   --      Selection containing the nodes to be layouted
   --      The selection will not be modified during the run
   --
   --    Widget:
   --      Graph_Widget where the nodes to layout reside
   --
   --    Widget_Lock:
   --      Lock for the widget - to be relased after the layout has finished
   --      it is not released, if an error at the production of an algorithm
   --      occurs
   --      The selection is locked by the caller (i.e. the lock is set and
   --        the factory has not to care about setting the lock)
   --
   --    Additonal_Parameters:
   --       String containing special parameters
   --       for the given layout algorithm. The layout
   --       algorithm for itself is responsible
   --       to parse them.
   --
   --    Layout_Evolution':
   --       Access to an Initialized Evolution-Object
   --       which does the layout null if unsuccessful
   --
   --
   --  Raises:
   --    Invalid_Format - if the string couldn't be parsed properly
   --                     The description of the exception contains the
   --                     error description
   --
   --  Currently implemented algorithms with their additional parameters:
   --
   --  "matrix":
   --    Format:  <Target_Position>
   --    Example: "(2,2)"
   --
   --    Meaning:
   --      <Target_Position>  upper left corner of matrix
   --
   --  "tree":
   --     Format:  <Root_Node_ID>; <Target_Position>; <List_Of_Class_Set_Names>
   --     Example: "5; (2,5); ("Aber", "Hallo")"
   --              It is not possible to use " or ; in a classsetname!
   --
   --     Meaning:
   --      Root_Node_Id       : The root-node of the tree to layout
   --      Class_Set_Names    : Names of ClassSet containing node-classes
   --                           and edge-classes to layout
   --      Target_Position    : Position on window, where the root-node has to
   --                           be placed
   procedure Create
     (Algorithm             : in     String;
      Selection_To_Layout   : in     Graph_Lib.Selections.Selection;
      Widget                : in     Graph_Widgets.Graph_Widget;
      Widget_Lock           : in     Graph_Widgets.Lock_Type;
      Additional_Parameters : in     String;
      Layout_Evolution      :    out Evolutions.Evolution_Class_Access);

private

end Giant.Layout_Factory;
