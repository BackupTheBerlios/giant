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
--  $RCSfile: giant-data_clists.ads,v $, $Revision: 1.4 $
--  $Author: keulsn $
--  $Date: 2003/09/12 20:30:12 $
--
--  Provides an convenince Gtk.Clist that has a single row data type
--  associated.
--
--  Pattern:
--    ADT
--

with Glib;
with Gtk.Clist;

with Giant.Clists;

generic

   type Data_Type (<>) is private;

package Giant.Data_Clists is

   type Giant_Data_Clist_Record is new Clists.Giant_Clist_Record with private;

   type Giant_Data_Clist is access all Giant_Data_Clist_Record'Class;

   type Update_Procedure_Type is access procedure
     (List : access Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Item : in     Data_Type);

   package Data is new Gtk.Clist.Row_Data (Data_Type);

   procedure Create
     (List             :    out Giant_Data_Clist;
      Columns          : in     Glib.Gint;
      Update_Procedure : in     Update_Procedure_Type);

   procedure Initialize
     (List             : access Giant_Data_Clist_Record'Class;
      Columns          : in     Glib.Gint;
      Update_Procedure : in     Update_Procedure_Type);

   procedure Add
     (List : access Giant_Data_Clist_Record;
      Item : in     Data_Type);

   function Get_Row
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type)
      return Glib.Gint;

   function Get_Selected_Item
     (List : access Giant_Data_Clist_Record)
      return Data_Type;

   procedure Update
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type);

   procedure Remove
     (List : access Giant_Data_Clist_Record;
      Item : in Data_Type);

private

   type Giant_Data_Clist_Record is new Clists.Giant_Clist_Record with record
      Update_Procedure : Update_Procedure_Type;
   end record;

end Giant.Data_Clists;
