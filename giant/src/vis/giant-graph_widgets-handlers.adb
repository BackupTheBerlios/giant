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
--  $RCSfile: giant-graph_widgets-handlers.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/29 14:00:49 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;
with System;

package body Giant.Graph_Widgets.Handlers is

   type Button_Press_Action_Access is access constant Button_Press_Action;

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


   procedure Emit_Action_Mode_Button_Press_Event
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is

      function To_Address is new Ada.Unchecked_Conversion
        (Source => Button_Press_Action_Access, Target => System.Address);
      User_Action : aliased constant Button_Press_Action := (Event, Location);
   begin
      Action_Mode_Marshallers.Emit_By_Name
        (Object => Widget,
         Name   => Action_Mode_Button_Press_Event,
         Param  => To_Address (User_Action'Unchecked_Access));
   end Emit_Action_Mode_Button_Press_Event;


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
     Action_Mode_Button_Press_Event +
     Logical_Area_Changed_Signal +
     Visible_Area_Changed_Signal;

   function Get_Signal_Array
     return Gtkada.Types.Chars_Ptr_Array is
   begin
      return Signal_Array;
   end Get_Signal_Array;

end Giant.Graph_Widgets.Handlers;
