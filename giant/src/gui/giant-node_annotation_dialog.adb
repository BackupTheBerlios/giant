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
--  $RCSfile: giant-node_annotation_dialog.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Glib;
with Gtk.Button;
with Gtk.Gentry;
with Gtk.Scrolled_Window;
with Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;

with Giant.Controller;
with Giant.Gui_Utils;

package body Giant.Node_Annotation_Dialog is

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   procedure On_Delete_Button_Clicked
     (Source : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Dialog : Node_Annotation_Dialog_Access;
   begin
      Dialog := Node_Annotation_Dialog_Access (Gtk.Widget.Get_Toplevel
                                               (Gtk.Widget.Gtk_Widget
                                                (Source)));
      Controller.Remove_Node_Annotation (Dialog.Node);
      Default_Dialog.Hide (Source, Default_Dialog.Response_Close);
   end On_Delete_Button_Clicked;

   function Can_Hide
     (Dialog : access Node_Annotation_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay) then
         declare
            Text : constant String := Gtk.Text.Get_Chars (Dialog.Text_Area);
         begin
            Controller.Set_Node_Annotation (Dialog.Node, Text);
         end;
      end if;

      return True;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog :    out Node_Annotation_Dialog_Access)
   is
   begin
      Dialog := new Node_Annotation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   procedure Initialize
     (Dialog : access Node_Annotation_Dialog_Record'class)
   is
      use Giant.Gui_Utils;

      Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   begin
      Default_Dialog.Initialize (Dialog,
                                 -"Annotate Node",
                                 Default_Dialog.Button_Okay_Cancel);

      -- text area
      Gtk.Text.Gtk_New (Dialog.Text_Area);
      Gtk.Text.Set_Editable (Dialog.Text_Area, True);

      Gtk.Scrolled_Window.Gtk_New (Scrolled_Window);
      Gtk.Scrolled_Window.Set_Policy (Scrolled_Window, Policy_Automatic,
                                      Policy_Always);
      Gtk.Scrolled_Window.Add (Scrolled_Window, Dialog.Text_Area);

      Set_Center_Widget (Dialog, Scrolled_Window);
      Set_Default (Dialog, Dialog.Text_Area);

      -- buttons
      Add_Button (Dialog,
                  New_Button (-"Delete", On_Delete_Button_Clicked'Access));
   end;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   procedure Set_Node
     (Dialog : access Node_Annotation_Dialog_Record'Class;
      Node   : in     Graph_Lib.Node_Id)
   is
      Position : Glib.Gint := 0;
   begin
      Dialog.Node := Node;

      Gtk.Text.Delete_Text (Dialog.Text_Area);
      Gtk.Text.Insert_Text (Dialog.Text_Area,
                            Controller.Get_Node_Annotation (Node),
                            Position);
   end Set_Node;

   procedure Show
     (Node : in Graph_Lib.Node_Id)
   is
      Dialog : Node_Annotation_Dialog_Access;
   begin
      Create (Dialog);
      Set_Node (Dialog, Node);
      Show_Modal (Dialog);
      Destroy (Dialog);
   end Show;

end Giant.Node_Annotation_Dialog;
