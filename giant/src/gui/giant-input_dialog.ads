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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-input_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides an input dialog.
--

with Gtk.Gentry;
with Gtk.Widget;

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

   procedure On_Input_Activated
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class);

   type Input_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Input : Gtk.Gentry.Gtk_Entry;
        Input_Validator : Input_Validator_Type;
        Custom_Data : Data_Type;
     end record;

end Giant.Input_Dialog;
