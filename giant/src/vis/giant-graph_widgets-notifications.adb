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
--  $RCSfile: giant-graph_widgets-notifications.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/29 14:00:04 $
--
------------------------------------------------------------------------------


with Giant.Graph_Widgets.Handlers;
with Giant.Logger;

package body Giant.Graph_Widgets.Notifications is

   package Notification_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Notifications");

   procedure Edge_Popup
     (Widget : access Graph_Widget_Record'Class;
      Edge   : in     Vis_Data.Vis_Edge_Id) is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Notification_Logger.Debug
        ("User requested PopUp on edge - Unimplemented.");
   end Edge_Popup;

   procedure Node_Popup
     (Widget : access Graph_Widget_Record'Class;
      Node   : in     Vis_Data.Vis_Node_Id) is

      Graph_Node : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
   begin
      Notification_Logger.Debug
        ("User requested PopUp on node "
         & Graph_Lib.Node_Id_Image (Graph_Node) & " - Unimplemented.");
   end Node_Popup;

   procedure Selection_Changed
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection) is
   begin
      Notification_Logger.Debug
        ("User has performed the command "
         & Selection_Change_Type'Image (Action) & " on the current selection."
         & " - Unimplemented.");
   end Selection_Changed;

   procedure Action_Mode_Button_Press_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is
   begin
      Notification_Logger.Debug
        ("User has pressed a mouse button during action mode."
         & " - Unimplemented.");
      Handlers.Emit_Action_Mode_Button_Press_Event (Widget, Event, Location);
   end Action_Mode_Button_Press_Event;

   procedure Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      Notification_Logger.Debug
        ("Logical area has changed. Emitting "
         & Handlers.Logical_Area_Changed_Signal);
      Handlers.Emit_Logical_Area_Changed (Widget, Area);
   end Logical_Area_Changed;

   procedure Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      Notification_Logger.Debug
        ("Visible area has changed. Emitting "
         & Handlers.Visible_Area_Changed_Signal);
      Handlers.Emit_Visible_Area_Changed (Widget, Area);
   end Visible_Area_Changed;


end Giant.Graph_Widgets.Notifications;
