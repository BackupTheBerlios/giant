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
--  $RCSfile: giant-generate_subgraph_dialog.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Giant.Default_Dialog;
with Giant.Gui_Utils;
with Giant.Graph_Lib.Subgraphs;

with Gtk.Gentry;

package Giant.Generate_Subgraph_Dialog is

   type Generate_Subgraph_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Generate_Subgraph_Dialog_Access is
      access all Generate_Subgraph_Dialog_Record'Class;

   procedure Show
     (The_Subgraph : in Graph_Lib.Subgraphs.Subgraph);

private
   type Generate_Subgraph_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
         The_Subgraph      : Graph_Lib.Subgraphs.Subgraph;
         Class_Set_List    : Gui_Utils.String_Clists.Giant_Data_Clist;
         New_Subgraph_Name : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Generate_Subgraph_Dialog;
