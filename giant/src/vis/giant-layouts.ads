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
--  $RCSfile: giant-layouts.ads,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/06/10 00:30:41 $
--
------------------------------------------------------------------------------
--
--  Contains the basic things needed of the layout-algorithms
--

with Giant.Evolutions;
with Giant.Graph_Widgets;
with Giant.Graph_Lib.Selections;

package Giant.Layouts is

   type Create_Result is
     (Successful,
      Algorithm_Not_Found,
      Additonal_Parameters_Error,
      Other_Error);

   ----------------------------------------------------------------------------
   --  Initialises an layout-algorithm
   --
   --  The resulting object has to be destroyed after usage
   --
   --  Parameters:
   --    Algorithm            : String, which describes the algorithm to be
   --                           used
   --    Selection_To_Layout  : Selection containing the nodes to be layouted
   --                           There will be no copy made. The
   --                           caller has to assure that the selection is
   --                           available and unmodified during the whole run
   --    Widget_Containing_Selection : Graph_Widet where the nodes to layout
   --                                  reside
   --    Additonal_Parameters : String containing special parameters
   --                           for the given layout algorithm. The layout
   --                           algorithm for itself is responsible
   --                           to parse them.
   --    Additonal_Parameters_Error : If there was an error parsing the
   --                                 additonal parameters
   --    Layout_Evolution'    : Access to an Initialized Evolution-Object
   --                           which does the layout
   --                           null if unsuccessful
   --    Result'              : Result stating how successful it was
   procedure Create
     (Algorithm                   : in     String;
      Selection_To_Layout         :
        in     Giant.Graph_Lib.Selections.Selection;
      Widget_Containing_Selection : in     Giant.Graph_Widgets.Graph_Widget;
      Additional_Parameters       : in     String;
      Additional_Parameters_Error :    out String;
      Layout_Evolution            :
        out Giant.Evolutions.Evolution_Class_Access;
      Result                      :    out Create_Result
     );

end Giant.Layouts;
