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
--  $RCSfile: giant-default_dialog.ads,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides a default dialog with a content area and a button panel.
--

with Gtk.Box;
with Gtk.Button;
with Gtk.Hbutton_Box;
with Gtk.Widget;
with Gtk.Window;
with Gtkada.Types;

package Giant.Default_Dialog is

   type Input_Callback is access function (S: in String) return Boolean;

   type Response_Type is
      (Response_Okay, Response_Cancel, Response_Close, Response_Yes,
       Response_No);

   type Button_Type is
     (Button_None, Button_Cancel, Button_Close, Button_Okay,
      Button_Okay_Cancel, Button_Yes_No, Button_Yes_No_Cancel);

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with private;

   type Default_Dialog_Access is access all Default_Dialog_Record'Class;

   procedure Create
     (Dialog  :    out Default_Dialog_Access;
      Title   : in     String;
      Buttons : in     Button_Type);

   procedure Initialize
     (Dialog  : access Default_Dialog_Record'class;
      Title   : in     String;
      Buttons : in     Button_Type);

   procedure Add_Button
     (Dialog   : access Default_Dialog_Record;
      Button   : in     Gtk.Button.Gtk_Button;
      Add_Left : in     Boolean               := True);


   function Add_Icon_Box
     (Dialog        : access Default_Dialog_Record;
      Icon_Filename : in     String;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox;

   function Add_Icon_Box
     (Dialog : access Default_Dialog_Record;
      Pixmap : in     Gtkada.Types.Chars_Ptr_Array;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Box.Gtk_Hbox;

   ---------------------------------------------------------------------------
   --  Invoked by Giant.Dialogs to create the default dialogs.
   --
   --  The message is displayed in a Gtk_Label.
   --
   --  Returns:
   --    The created box.
   function Add_Icon_Box
     (Dialog  : access Default_Dialog_Record;
      Pixmap  : in     Gtkada.Types.Chars_Ptr_Array;
      Message : in     String                       := "")
     return Gtk.Box.Gtk_Hbox;

   ---------------------------------------------------------------------------
   --  Called if one of the default buttons is pressed.
   --
   --  Sub-classes can overwrite this method.
   --
   --  Returns:
   --    True
   function Can_Hide
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   function Get_Center_Box
     (Dialog : access Default_Dialog_Record)
     return Gtk.Box.Gtk_Vbox;

   function Get_Response
     (Dialog : access Default_Dialog_Record)
     return Response_Type;

   ---------------------------------------------------------------------------
   --  If dialog is not modal, the dialog is destroyed.
   --
   procedure Hide
     (Dialog : access Default_Dialog_Record);

   ---------------------------------------------------------------------------
   --  Called by the button callbacks.
   procedure Hide
     (Source   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response : in     Response_Type);

   ---------------------------------------------------------------------------
   --  Returns true, if the dialog runs in a separate gtk event loop.
   function Is_Modal
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns true, if the okay button was pressed.
   function Is_Response_Okay
     (Dialog : access Default_Dialog_Record)
     return Boolean;

   procedure Set_Center_Widget
     (Dialog : access Default_Dialog_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Show_Modal
     (Dialog : access Default_Dialog_Record);

private

   type Default_Dialog_Record is new Gtk.Window.Gtk_Window_Record with record
      Center_Box : Gtk.Box.Gtk_Vbox;
      Button_Box : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Response : Response_Type;
      Is_Modal : Boolean := False;
   end record;

end Giant.Default_Dialog;
