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
--  $RCSfile: giant-clists.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/19 16:38:06 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Glib; use type Glib.Gint;
with Gtk.Enums; 
with Gtkada.Types;

package body Giant.Clists is

   procedure Create
	 (List			   :    out Giant_Clist;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  List := new Giant_Clist_Record;
	  Initialize (List, Columns, Update_Procedure);
   end Create;

   procedure Initialize
	 (List			   : access Giant_Clist_Record'Class;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  Gtk.Clist.Initialize (List, Columns);

	  List.Update_Procedure := Update_Procedure;

	  Set_Selection_Mode (List, Gtk.Enums.Selection_Single);
      Set_Show_Titles (List, True);
	  Column_Titles_Active (List);
   end;

   procedure Add
	 (List : access Giant_Clist_Record;
	  Item : in     Data_Type)
   is
      use Gtkada.Types;
 
      Row_Data : Gtkada.Types.Chars_Ptr_Array
		(0 .. Interfaces.C.Size_t (Get_Columns (List)));
      Row : Glib.Gint;
   begin
	  --  append row with dummy data
      for I in Row_Data'Range loop
         Row_Data (I) := Interfaces.C.Strings.New_String ("");
	  end loop;
      Row := Append (List, Row_Data);
      Free (Row_Data);

	  --  set custom data
      Data.Set (List, Row, Item);

	  --  update row
      List.Update_Procedure (List, Row, Item);
   end Add;

   function Get_Row
	 (List : access Giant_Clist_Record;
	  Item : in Data_Type)
	  return Glib.Gint
   is
   begin
	  for I in 0 .. Get_Rows (List) - 1 loop
		 if (Data.Get (List, I) = Item) then
			return I;
		 end if;
	  end loop;
	  
	  return -1;
   end Get_Row;

   function Get_Selected_Item
	 (List : access Giant_Clist_Record)
	  return Data_Type
   is
   begin
	  return Data.Get (List, Get_Selected_Row (List));
   end Get_Selected_Item;

   function Get_Selected_Row
     (List : access Giant_Clist_Record'Class)
     return Glib.Gint
   is
      use type Gtk.Enums.Gint_List.Glist;
      Selection : constant Gtk.Enums.Gint_List.Glist := Get_Selection (List);
   begin
      if Selection /= Gtk.Enums.Gint_List.Null_List then
         return Gtk.Enums.Gint_List.Get_Data 
		   (Gtk.Enums.Gint_List.First (Selection));
      end if;

      return -1;
   end Get_Selected_Row;

   procedure Update
	 (List : access Giant_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
         List.Update_Procedure (List, Row, Item);
      end if;
   end Update;

   procedure Remove
	 (List : access Giant_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
		 Remove (List, Row);
      end if;
   end Remove;

end Giant.Clists;
