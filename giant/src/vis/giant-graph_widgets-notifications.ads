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
--  $RCSfile: giant-graph_widgets-notifications.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--
--  This Package manages the notification mechanism in a graph widget.
--  The graph widget calles the subprograms in this package. These
--  subprograms then forward the notifications to the graph widget's clients.
--


package Giant.Graph_Widgets.Notifications is


   -------------------
   -- PopUp Menus --
   -------------------

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has requested a PopUp Menu for
   --  one specific edge.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Edge   - The edge, the PopUp Menu is requested for
   procedure Edge_Popup
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Graph_Lib.Edge_Id);

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has requested a PopUp Menu for
   --  one specific node.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Node   - The node, the PopUp Menu is requested for
   procedure Node_Popup
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Graph_Lib.Node_Id);


   -----------------------
   -- Selection Changes --
   -----------------------

   ----------------------------------------------------------------------------
   --  Actions the user can perform on the current selection
   --
   --  Enumeration Literals:
   --    Insert - Request to insert certain edges and/or nodes from the
   --             current selection
   --    Remove - Request to remove certain edges and/or nodes from the
   --             current selection
   --    Change - Request to change the contents of the current selection
   --    Clear  - Request to clear the current selection
   type Selection_Change_Type is (Insert, Remove, Change, Clear);

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has given the command to modify
   --  the current selection.
   --
   --  Parameters:
   --    Widget     - The graph widget
   --    Action     - The user's change request
   --    Difference - The Selection necessary to service the request:
   --                 * Insert --> edges and nodes to be inserted
   --                 * Remove --> edges and nodes to be removed
   --                 * Change --> new content
   --                 * Clear  --> null
   procedure Selection_Changed
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection);


   ----------------
   -- Crosshairs --
   ----------------

   ----------------------------------------------------------------------------
   --  Actions, user can perform when in crosshair mode
   --
   --  Enumeration Literals:
   --    Cancel - The crosshair mode was canceled by the user
   --    Fire   - The user has used the crosshair
   type Crosshair_Action_Type is (Cancel, Fire);

   ----------------------------------------------------------------------------
   --  Informs the controller that the user has performed an action while
   --  in crosshair-mode
   --
   --  Parameters:
   --    Widget   - The graph widget
   --    Action   - The crosshair action the user has performed
   --    Location - The location where the crosshair action was performed
   --  Returns:
   --    False if 'Widget' should remain in crosshair mode, True if crosshair
   --    mode should be disabled.
   function Crosshair_Action
     (Widget   : access Graph_Widget_Record'Class;
      Action   : in     Crosshair_Action_Type;
      Location : in     Vis.Logic.Vector_2d)
     return Boolean;


   --------------------------
   -- Visible Area Changes --
   --------------------------

   ----------------------------------------------------------------------------
   --  Emits the Graph_Widgets.Handlers.Visible_Area_Changed_Signal
   --  to inform any listener that the visible logical area has changed.
   --
   --  Parameters:
   --    Widget - The graph widget
   --    Area   - The visible area inside 'Widget'
   procedure Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d);

end Giant.Graph_Widgets.Notifications;
