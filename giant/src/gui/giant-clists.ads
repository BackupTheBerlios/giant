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
--  $RCSfile: giant-clists.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
--  Provides an enhanced Gtk.Clist.
--
--  Pattern:
--    ADT
--

with Glib;
with Gtk.Clist;
with Gtk.Menu;

package Giant.Clists is

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with private;

   type Giant_Clist is access all Giant_Clist_Record'Class;

   procedure Create
     (List    :    out Giant_Clist;
      Columns : in     Glib.Gint);

   procedure Initialize
     (List    : access Giant_Clist_Record'Class;
      Columns : in     Glib.Gint);

   procedure Columns_Autosize
     (List  : access Giant_Clist_Record);

   procedure Connect_Popup_Menu
     (List : access Giant_Clist_Record;
      Menu : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Selected_Row
     (List : access Giant_Clist_Record)
     return Glib.Gint;

private

   type Giant_Clist_Record is new Gtk.Clist.Gtk_Clist_Record with null
      record;

end Giant.Clists;
