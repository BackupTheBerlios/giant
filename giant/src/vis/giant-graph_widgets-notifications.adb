------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets-notifications.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------


with Giant.Graph_Widgets.Handlers;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Notifications is

   package Notification_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Notifications");

   procedure Background_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is
   begin
      Notification_Logger.Debug
        ("Background_Popup at Location " & Vis.Logic.Image (Location));
      Handlers.Emit_Background_Popup_Event (Widget, Event, Location);
   end Background_Popup;

   procedure Edge_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id) is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Notification_Logger.Debug
        ("Edge_Popup node " &
         Graph_Lib.Node_Id_Image (Graph_Lib.Get_Source_Node (Graph_Edge))  &
         "'s edge " & Graph_Lib.Get_Edge_Tag (Graph_Edge));
      Handlers.Emit_Edge_Popup_Event (Widget, Event, Location, Graph_Edge);
   end Edge_Popup;

   procedure Node_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id) is

      Graph_Node : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
   begin
      Notification_Logger.Debug
        ("Node_Popup on " & Graph_Lib.Node_Id_Image (Graph_Node));
      Handlers.Emit_Node_Popup_Event (Widget, Event, Location, Graph_Node);
   end Node_Popup;

   procedure Selection_Changed
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection) is
   begin
      --  Notification_Logger.Debug
      --    ("User has performed the command " &
      --     Selection_Change_Type'Image (Action) & " on the current " &
      --     "selection. emitting " & Handlers.Selection_Change_Signal);
      Handlers.Emit_Selection_Change_Signal
        (Widget     => Widget,
         Action     => Action,
         Difference => Difference);
   end Selection_Changed;

   procedure Action_Mode_Button_Press_Event_Background
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button on the background during " &
      --     "action mode. Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Background
        (Widget, Event, Location);
   end Action_Mode_Button_Press_Event_Background;

   procedure Action_Mode_Button_Press_Event_Edge
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button on an edge during action " &
      --     "mode Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Edge
        (Widget, Event, Location, Vis_Data.Get_Graph_Edge (Edge));
   end Action_Mode_Button_Press_Event_Edge;

   procedure Action_Mode_Button_Press_Event_Node
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button during action mode." &
      --     " Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Node
        (Widget, Event, Location, Vis_Data.Get_Graph_Node (Node));
   end Action_Mode_Button_Press_Event_Node;

   procedure Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("Logical area has changed. Emitting "
      --     & Handlers.Logical_Area_Changed_Signal);
      Handlers.Emit_Logical_Area_Changed (Widget, Area);
   end Logical_Area_Changed;

   procedure Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("Visible area has changed. Emitting "
      --     & Handlers.Visible_Area_Changed_Signal);
      Handlers.Emit_Visible_Area_Changed (Widget, Area);
   end Visible_Area_Changed;


end Giant.Graph_Widgets.Notifications;
