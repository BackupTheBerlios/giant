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
--  $RCSfile: giant-data_clists.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Glib; use type Glib.Gint;
with Gtk.Enums; 
with Gtkada.Types;

package body Giant.Data_Clists is

   procedure Create
	 (List			   :    out Giant_Data_Clist;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  List := new Giant_Data_Clist_Record;
	  Initialize (List, Columns, Update_Procedure);
   end Create;

   procedure Initialize
	 (List			   : access Giant_Data_Clist_Record'Class;
	  Columns		   : in     Glib.Gint;
	  Update_Procedure : in     Update_Procedure_Type)
   is
   begin
	  Clists.Initialize (List, Columns);

	  List.Update_Procedure := Update_Procedure;
   end;

   procedure Add
	 (List : access Giant_Data_Clist_Record;
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
	 (List : access Giant_Data_Clist_Record;
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
	 (List : access Giant_Data_Clist_Record)
	  return Data_Type
   is
   begin
	  return Data.Get (List, Get_Selected_Row (List));
   end Get_Selected_Item;

   procedure Update
	 (List : access Giant_Data_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
         List.Update_Procedure (List, Row, Item);
      end if;
   end Update;

   procedure Remove
	 (List : access Giant_Data_Clist_Record;
	  Item : in Data_Type)
   is
      Row : Glib.Gint := Get_Row (List, Item);
   begin
      if (Row /= -1) then
		 Remove (List, Row);
      end if;
   end Remove;

end Giant.Data_Clists;
