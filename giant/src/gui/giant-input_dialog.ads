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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-input_dialog.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/08/26 16:07:16 $
--
------------------------------------------------------------------------------
--
-- Provides an input dialog.
--

with Gtk.Gentry;

with Giant.Default_Dialog;

generic

   type Data_Type is private;

package Giant.Input_Dialog is

   pragma Elaborate_Body;

   type Input_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Input_Dialog_Access is access all Input_Dialog_Record'Class;

   type Data_Type_Access is access Data_Type;

   ----------------------------------------------------------------------------
   --  Invoked when the user presses Okay.
   --
   --  Returns:
   --    True, if the value is valid and the dialog can be closed
   type Input_Validator_Type is access function
     (Text : in String;
      Data : in Data_Type)
     return Boolean;

   procedure Create
     (Dialog          :    out Input_Dialog_Access;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type);

   procedure Initialize
     (Dialog          : access Input_Dialog_Record'Class;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type);

   function Can_Hide
     (Dialog : access Input_Dialog_Record)
     return Boolean;

   function Get_Text
     (Dialog : access Input_Dialog_Record)
     return String;

   procedure Set_Text
     (Dialog : access Input_Dialog_Record;
      Text   : in     String);

   ----------------------------------------------------------------------------
   --  Returns the string the user has provided.
   --
   --  Returns:
   --    The empty string, if Cancel was pressed
   function Show
     (Message         : in String;
      Title           : in String               := -"Giant Input";
      Default_Input   : in String               := "";
      Input_Validator : in Input_Validator_Type := null;
      Custom_Data     : in Data_Type)
      return String;

private
   type Input_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Input : Gtk.Gentry.Gtk_Entry;
        Input_Validator : Input_Validator_Type;
        Custom_Data : Data_Type;
     end record;

end Giant.Input_Dialog;
