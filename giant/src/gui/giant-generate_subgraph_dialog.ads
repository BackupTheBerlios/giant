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
--  $RCSfile: giant-generate_subgraph_dialog.ads,v $, $Revision: 1.1 $
--  $Author: koppor $
--  $Date: 2003/10/07 17:11:49 $
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
