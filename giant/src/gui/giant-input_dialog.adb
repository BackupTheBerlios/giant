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
--  $RCSfile: giant-input_dialog.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--

with Gtk.Button;
with Gtk.Box;
with Gtk.Widget;
with Gtkada.Pixmaps;

with Giant.Gui_Utils;

package body Giant.Input_Dialog is

   procedure On_Input_Activated
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Default_Dialog.Hide (Source, Default_Dialog.Response_Okay);
   end On_Input_Activated;

   procedure Create
     (Dialog          :    out Input_Dialog_Access;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type)
   is
   begin
      Dialog := new Input_Dialog_Record;
      Initialize (Dialog, Title, Message, Input_Validator, Custom_Data);
   end Create;

   procedure Initialize
     (Dialog          : access Input_Dialog_Record'class;
      Title           : in     String;
      Message         : in     String;
      Input_Validator : in     Input_Validator_Type;
      Custom_Data     : in     Data_Type)
   is
      Box : Gtk.Box.Gtk_Hbox;
   begin
      Default_Dialog.Initialize (Dialog, Title,
                                 Default_Dialog.Button_Okay_Cancel);

      Dialog.Input_Validator := Input_Validator;
      Dialog.Custom_Data := Custom_Data;

      Box := Add_Icon_Box (Dialog, Gtkada.Pixmaps.Confirmation_Xpm, Message);

      Gtk.Gentry.Gtk_New (Dialog.Input);
      Gui_Utils.Widget_Callback.Object_Connect
           (Dialog.Input, "activate",
            Gui_Utils.Widget_Callback.To_Marshaller (On_Input_Activated'Access),
            Dialog);
      Gtk.Box.Add (Box, Dialog.Input);

      Gtk.Gentry.Set_Flags (Dialog.Input, Gtk.Widget.Can_Default);
      Gtk.Gentry.Grab_Default (Dialog.Input);
   end Initialize;

   function Can_Hide
     (Dialog : access Input_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog)
          = Default_Dialog.Response_Okay) then
         if (Dialog.Input_Validator /= null) then
            return Dialog.Input_Validator (Get_Text (Dialog),
                                           Dialog.Custom_Data);
         end if;
      end if;

      return True;
   end Can_Hide;

   function Get_Text
     (Dialog : access Input_Dialog_Record)
     return String
   is
      Text : constant String := Gtk.Gentry.Get_Text (Dialog.Input);
   begin
      return Text;
   end Get_Text;

   procedure Set_Text
     (Dialog : access Input_Dialog_Record;
      Text   : in     String)
   is
   begin
      Gtk.Gentry.Set_Text (Dialog.Input, Text);
   end Set_Text;

   function Show
     (Message         : in String;
      Title           : in String               := -"Giant Input";
      Default_Input   : in String               := "";
      Input_Validator : in Input_Validator_Type := null;
      Custom_Data     : in Data_Type)
      return String
   is
      use type Default_Dialog.Response_Type;

      Dialog : Input_Dialog_Access;
   begin
      Input_Dialog.Create (Dialog, Title, Message, Input_Validator,
                           Custom_Data);
      Input_Dialog.Set_Text (Dialog, Default_Input);

      Input_Dialog.Show_Modal (Dialog);

      if (Input_Dialog.Get_Response (Dialog)
          = Default_Dialog.Response_Okay) then
         declare
            S : constant String := Input_Dialog.Get_Text (Dialog);
         begin
            Input_Dialog.Destroy (Dialog);
            return S;
         end;
      else
         Input_Dialog.Destroy (Dialog);
         return "";
      end if;
   end Show;

end Giant.Input_Dialog;
