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
--  $RCSfile: giant-node_info_dialog.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Gtk.Label;

with Giant.Clists;
with Giant.Default_Dialog;
with Giant.Graph_Lib;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Window;
with Giant.Vis;

package Giant.Node_Info_Dialog is

   type Node_Info_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Node_Info_Dialog_Access is
      access all Node_Info_Dialog_Record'Class;

   function Can_Hide
     (Dialog : access Node_Info_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog : out Node_Info_Dialog_Access);

   procedure Initialize
     (Dialog : access Node_Info_Dialog_Record'Class);

   procedure Set_Node
     (Dialog : access Node_Info_Dialog_Record;
      Node   : in     Graph_Lib.Node_Id);

   procedure Show
     (Node : in Graph_Lib.Node_Id);

   package Actions is
      type Pick_Node_Action_Type is
        new Graph_Window.Actions.Graph_Window_Action_Type with record
           Dialog : Node_Info_Dialog_Access;
        end record;

      type Pick_Node_Action_Access is
        access all Pick_Node_Action_Type'Class;

      function Create
        (Dialog : in Node_Info_Dialog_Access)
        return Pick_Node_Action_Access;

      procedure Cancel
        (Action : access Pick_Node_Action_Type);

      function Execute
        (Action   : access Pick_Node_Action_Type;
         Window   : access Graph_Window.Graph_Window_Record'Class;
         Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
        return Boolean;
   end Actions;

private
   type Node_Info_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Pick_Action : Actions.Pick_Node_Action_Access;
        ID_Label : Gtk.Label.Gtk_Label;
        Type_Label : Gtk.Label.Gtk_Label;
        Attribute_List : Clists.Giant_Clist;
        Successor_List : Clists.Giant_Clist;
        Predecessor_List : Clists.Giant_Clist;
        Node : Graph_Lib.Node_Id;
     end record;

end Giant.Node_Info_Dialog;
