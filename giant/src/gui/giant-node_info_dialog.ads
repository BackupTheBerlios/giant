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
--  $RCSfile: giant-node_info_dialog.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/08/12 13:14:05 $
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

private
   type Node_Info_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        ID_Label : Gtk.Label.Gtk_Label;
        Type_Label : Gtk.Label.Gtk_Label;
        Attribute_List : Clists.Giant_Clist;
        Successor_List : Clists.Giant_Clist;
        Predecessor_List : Clists.Giant_Clist;
        Node : Graph_Lib.Node_Id;
     end record;

end Giant.Node_Info_Dialog;
