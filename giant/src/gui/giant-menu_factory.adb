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
--  $RCSfile: giant-menu_factory.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Ada.Strings.Unbounded;

with Gtk.Handlers;
with Gtk.Menu_Item;

with String_Lists;

with Giant.Gui_Utils;
with Giant.String_Split;

package body Giant.Menu_Factory is

   procedure Generate
     (Labels    : in     String;
      Separator : in     String;
      Menu      : in     Gtk.Menu.Gtk_Menu;
      Callback  : in     Widget_User_Callback.Marshallers.Void_Marshaller.Handler;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Item : Ada.Strings.Unbounded.Unbounded_String;
      Data : Script_Event_Access;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      List := String_Split.Split_String (Labels, Separator);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Item);

         declare
            Label : constant String
              := Ada.Strings.Unbounded.To_String (Item);
         begin
            if (Label = "-") then
               Gtk.Menu.Append (Menu, Gui_Utils.New_Menu_Separator);
            else
               Data := new Script_Event(Label'Length);
               Data.Widget := Gtk.Widget.Gtk_Widget (Widget);
               Data.Label := Label;

               Gtk.Menu_Item.Gtk_New (Menu_Item, Data.Label);
               Widget_User_Callback.Connect
                 (Menu_Item,
                  "activate",
                  Widget_User_Callback.Marshallers.Void_Marshaller.To_Marshaller
                  (Callback),
                  Data.all);

               Gtk.Menu.Append (Menu, Menu_Item);
            end if;
         end;
      end loop;
      String_Lists.Destroy (List);
   end Generate;

end Giant.Menu_Factory;
