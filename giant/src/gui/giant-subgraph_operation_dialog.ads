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
--  $RCSfile: giant-subgraph_operation_dialog.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/18 14:27:40 $
--
------------------------------------------------------------------------------
--
--
--

with Giant.Set_Operation_Dialog;

package Giant.Subgraph_Operation_Dialog is

   type Subgraph_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with private;

   type Subgraph_Operation_Dialog_Access is
     access all Subgraph_Operation_Dialog_Record'Class;

   procedure Create
     (Dialog : out Subgraph_Operation_Dialog_Access);

   procedure Initialize
     (Dialog : access Subgraph_Operation_Dialog_Record'Class);
   
   function Can_Hide
     (Dialog : access Subgraph_Operation_Dialog_Record)
     return Boolean;

private
   
   type Subgraph_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with null record;

end Giant.Subgraph_Operation_Dialog;
