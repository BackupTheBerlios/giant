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
--  $RCSfile: giant-data_clists.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
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
