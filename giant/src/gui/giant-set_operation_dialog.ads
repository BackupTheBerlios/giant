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
--  $RCSfile: giant-set_operation_dialog.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/07/18 14:27:39 $
--
------------------------------------------------------------------------------
--
--
--

with Gtk.Combo;
with Gtk.Enums;
with Gtk.Gentry;

with Giant.Default_Dialog;

package Giant.Set_Operation_Dialog is

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Set_Operation_Dialog_Access is
     access all Set_Operation_Dialog_Record'Class;

   type Operation_Type is (Difference, Intersection, Union);

   Invalid_Operation_Entered : exception;

   procedure Create
     (Dialog :    out Set_Operation_Dialog_Access;
      Items  : in     Gtk.Enums.String_List.Glist);

   procedure Initialize
     (Dialog : access Set_Operation_Dialog_Record'Class;
      Items  : in     Gtk.Enums.String_List.Glist);

   function Get_Left_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Right_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Operation
     (Dialog : access Set_Operation_Dialog_Record)
     return Operation_Type;

   function Get_Target
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Validate
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean;

private

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Left_Source : Gtk.Combo.Gtk_Combo;
        Right_Source : Gtk.Combo.Gtk_Combo;
        Operation : Gtk.Combo.Gtk_Combo;
        Target : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Set_Operation_Dialog;
