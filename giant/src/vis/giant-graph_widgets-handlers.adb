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
--  $RCSfile: giant-graph_widgets-handlers.adb,v $, $Revision: 1.7 $
--  $Author: keulsn $
--  $Date: 2003/08/19 12:42:32 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;
with System;

package body Giant.Graph_Widgets.Handlers is

   type Button_Press_Action_Access is access constant Button_Press_Action;

   type Selection_Change_Action_Access is access constant
     Selection_Change_Action;

   type Rectangle_Access is access constant Vis.Logic.Rectangle_2d;


   --  Package for emitting signals. 'System.Address' must be used as
   --  'Base_Type' because the formal parameter 'Conversion' to the
   --  generic subprogram 'Emit_By_Name_Generic' cannot be implemented.
   --
   --  It would be desirable to have 'Base_Type => Button_Press_Action', but
   --
   --  (1)  The argument 'Param' to 'Emit_By_Name_Generic' is of type
   --       'Base_Type'. In 'Conversion' 'Param'Access' cannot be taken
   --       because 'Param' is not an aliased view.
   --  (2)  Even if 'Param'Access' could be taken then it is not assured
   --       that 'Param'Access' is meaningful after 'Conversion' has returned
   --       to its caller.
   package Action_Mode_Marshallers is new
     Action_Mode_Cbs.Marshallers.Generic_Marshaller
     (Base_Type  => System.Address,
      Conversion => Gtk.Arguments.Get_Nth);

   package Logical_Area_Marshallers is new
     Logical_Area_Cbs.Marshallers.Generic_Marshaller
     (Base_Type  => System.Address,
      Conversion => Gtk.Arguments.Get_Nth);

   package Visible_Area_Marshallers renames Logical_Area_Marshallers;


   function To_Rectangle_2d
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Vis.Logic.Rectangle_2d is

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Rectangle_Access);

      Rectangle : Rectangle_Access;
   begin
      Rectangle := Convert (Gtk.Arguments.Get_Nth (Args, Num));
      return Rectangle.all;
   end To_Rectangle_2d;


   function To_Button_Press_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Button_Press_Action is

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Button_Press_Action_Access);

      Action : Button_Press_Action_Access;
   begin
      Action := Convert (Gtk.Arguments.Get_Nth (Args, Num));
      return Action.all;
   end To_Button_Press_Action;

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Button_Press_Action_Access,
      Target => System.Address);


   function To_Selection_Change_Action
     (Args : in Gtk.Arguments.Gtk_Args;
      Num  : in Natural)
     return Selection_Change_Action is

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Selection_Change_Action_Access);

      Action : Selection_Change_Action_Access;
   begin
      Action := Convert (Gtk.Arguments.Get_Nth (Args, Num));
      return Action.all;
   end To_Selection_Change_Action;


   procedure Emit_Background_Popup_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is

      User_Action : aliased Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Background;
      --  'User_Action.Edge' remains uninitialized
      --  'User_Action.Node' remains uninitialized
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Background_Popup_Event,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Background_Popup_Event;

   procedure Emit_Edge_Popup_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Graph_Lib.Edge_Id) is

      User_Action : aliased Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Edge;
      User_Action.Edge := Edge;
      --  'User_Action.Node' remains uninitialized
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Edge_Popup_Event,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Edge_Popup_Event;

   procedure Emit_Node_Popup_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Graph_Lib.Node_Id) is

      User_Action : aliased Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Node;
      --  'User_Action.Edge' remains uninitialized
      User_Action.Node := Node;
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Node_Popup_Event,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Node_Popup_Event;

   procedure Emit_Selection_Change_Signal
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection) is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => Selection_Change_Action_Access, Target => System.Address);
      User_Action : aliased constant Selection_Change_Action :=
        (Action, Difference);
   begin
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Selection_Change_Signal,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Selection_Change_Signal;

   procedure Emit_Action_Mode_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Action : in     Button_Press_Action) is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => Button_Press_Action_Access, Target => System.Address);
      User_Action : aliased constant Button_Press_Action := Action;
   begin
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Action_Mode_Button_Press_Event,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Action_Mode_Button_Press_Event;

   procedure Emit_Action_Mode_Button_Press_Event_Background
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is

      User_Action : Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Background;
      --  'User_Action.Edge' remains uninitialized
      --  'User_Action.Node' remains uninitialized
      Emit_Action_Mode_Button_Press_Event
        (Widget => Widget,
         Action => User_Action);
   end Emit_Action_Mode_Button_Press_Event_Background;

   procedure Emit_Action_Mode_Button_Press_Event_Edge
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Graph_Lib.Edge_Id) is

      User_Action : Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Edge;
      User_Action.Edge := Edge;
      --  'User_Action.Node' remains uninitialized
      Emit_Action_Mode_Button_Press_Event
        (Widget => Widget,
         Action => User_Action);
   end Emit_Action_Mode_Button_Press_Event_Edge;

   procedure Emit_Action_Mode_Button_Press_Event_Node
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Graph_Lib.Node_Id) is

      User_Action : Button_Press_Action;
   begin
      User_Action.Event := Event;
      User_Action.Location := Location;
      User_Action.Pressed_On := On_Node;
      --  'User_Action.Edge' remains uninitialized
      User_Action.Node := Node;
      Emit_Action_Mode_Button_Press_Event
        (Widget => Widget,
         Action => User_Action);
   end Emit_Action_Mode_Button_Press_Event_Node;


   procedure Emit_Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => Rectangle_Access, Target => System.Address);
      Area_Slot : aliased constant Vis.Logic.Rectangle_2d := Area;
   begin
      Logical_Area_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Logical_Area_Changed_Signal,
         Param  => To_Address (Area_Slot'Unchecked_Access));
   end Emit_Logical_Area_Changed;


   procedure Emit_Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => Rectangle_Access, Target => System.Address);
      Area_Slot : aliased constant Vis.Logic.Rectangle_2d := Area;
   begin
      Visible_Area_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Visible_Area_Changed_Signal,
         Param  => To_Address (Area_Slot'Unchecked_Access));
   end Emit_Visible_Area_Changed;


   function "+"
     (Left  : String;
      Right : String)
     return Gtkada.Types.Chars_Ptr_Array
     renames Gtkada.Types."+";

   function "+"
     (Left  : Gtkada.Types.Chars_Ptr_Array;
      Right : String)
     return Gtkada.Types.Chars_Ptr_Array
     renames Gtkada.Types."+";

   ----------------------------------------------------------------------------
   --  Global Variable containing all Signals, never gets destroyed.
   Signal_Array : constant Gtkada.Types.Chars_Ptr_Array :=
     Set_Scroll_Adjustments_Signal +
     Background_Popup_Event +
     Edge_Popup_Event +
     Node_Popup_Event +
     Selection_Change_Signal +
     Action_Mode_Button_Press_Event +
     Logical_Area_Changed_Signal +
     Visible_Area_Changed_Signal;

   Signal_Parameters : constant Gtk.Object.Signal_Parameter_Types :=
     (--  Set_Scroll_Adjustments_Signal
      1 => (1 => Gtk.Gtk_Type_Object, 2 => Gtk.Gtk_Type_Object),
      --  Background_Popup_Event
      2 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Edge_Popup_Event
      3 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Node_Popup_Event
      4 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Selection_Chage_Siganl
      5 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Action_Mode_Button_Press_Event
      6 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Logical_Area_Changed_Signal
      7 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None),
      --  Visible_Area_Changed_Signal
      8 => (1 => Gtk.Gtk_Type_Pointer, 2 => Gtk.Gtk_Type_None));

   function Get_Signal_Array
     return Gtkada.Types.Chars_Ptr_Array is
   begin
      return Signal_Array;
   end Get_Signal_Array;

   function Get_Signal_Parameters
     return Gtk.Object.Signal_Parameter_Types is
   begin
      return Signal_Parameters;
   end Get_Signal_Parameters;

   --  returns the index for the "set_scroll_adjustments" signal in the
   --  array returned by 'Get_Signal_Array'
   function Get_Scroll_Adjustments_Signal
     return Glib.Guint is

      Value : Glib.Guint := 1;
   begin
      return Value;
   end Get_Scroll_Adjustments_Signal;

end Giant.Graph_Widgets.Handlers;
