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
--  $RCSfile: giant-node_annotation_dialog.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/23 11:30:45 $
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

   function Can_Hide
     (Dialog : access Node_Annotation_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog :    out Node_Annotation_Dialog_Access;
      Node   : in     Graph_Lib.Node_Id);

   procedure Initialize
     (Dialog : access Node_Annotation_Dialog_Record'Class);

   procedure Show
     (Node : in Graph_Lib.Node_Id);

private
   type Node_Annotation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Text_Area : Gtk.Text.Gtk_Text;
        Node : Graph_Lib.Node_Id;
     end record;

end Giant.Node_Annotation_Dialog;
