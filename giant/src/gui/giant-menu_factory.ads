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
--  $RCSfile: giant-menu_factory.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
--  Creates menu items for GSL script entries.
--

with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Widget;

package Giant.Menu_Factory is

   type Script_Event (Label_Length : Natural) is record
     Widget : Gtk.Widget.Gtk_Widget;
     Label  : String (1 .. Label_Length);
   end record;

   type Script_Event_Access is access Script_Event;

   package Widget_User_Callback is new
     Gtk.Handlers.User_Callback (Gtk.Widget.Gtk_Widget_Record,
                                 Script_Event);


   type Script_Callback_Type is access procedure
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Data   : in     Script_Event);

   procedure Generate
     (Labels    : in     String;
      Separator : in     String;
      Menu      : in     Gtk.Menu.Gtk_Menu;
      Callback  : in     Widget_User_Callback.Marshallers.Void_Marshaller.Handler;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

end Giant.Menu_Factory;
