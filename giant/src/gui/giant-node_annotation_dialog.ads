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
--  $RCSfile: giant-node_annotation_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Gtk.Text;

with Giant.Default_Dialog;
with Giant.Graph_Lib;

package Giant.Node_Annotation_Dialog is

   type Node_Annotation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Node_Annotation_Dialog_Access is
      access all Node_Annotation_Dialog_Record'Class;

   procedure Create
     (Dialog :    out Node_Annotation_Dialog_Access);

   procedure Initialize
     (Dialog : access Node_Annotation_Dialog_Record'Class);

   function Can_Hide
     (Dialog : access Node_Annotation_Dialog_Record)
     return Boolean;

   procedure Set_Node
     (Dialog : access Node_Annotation_Dialog_Record'Class;
      Node   : in     Graph_Lib.Node_Id);

   procedure Show
     (Node : in Graph_Lib.Node_Id);

private
   type Node_Annotation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Text_Area : Gtk.Text.Gtk_Text;
        Node : Graph_Lib.Node_Id;
     end record;

end Giant.Node_Annotation_Dialog;
