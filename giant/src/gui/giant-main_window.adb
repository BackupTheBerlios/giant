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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003/06/18 15:16:26 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Gdk.Event;
with Gdk.Types;
with Glib; use type Glib.Gint;
with Gtk.Box;
with Gtk.Clist;
pragma Elaborate_All (Gtk.Clist);
with Gtk.Enums; use Gtk.Enums;
with Gtk.Main;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Status_Bar;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Paned;
with Gtk.Tearoff_Menu_Item;
with Gtkada.File_Selection;
with Gtkada.Types;

with Giant.Controller;
with Giant.Gsl_Dialog;
with Giant.Gui_Manager;
with Giant.Gui_Utils; use Giant.Gui_Utils;
with Giant.Projects;
with Giant.Set_Operation_Dialog;

package body Giant.Main_Window is

   --  main window instance
   Window : Gtk.Window.Gtk_Window;

   package Window_List_Data is new
     Gui_Utils.Clist_Row_Data (String);
   Window_List_Menu : Gtk.Menu.Gtk_Menu;
   Window_List : Gtk.Clist.Gtk_Clist;

   package Subgraph_List_Data is new
     Gui_Utils.Clist_Row_Data (String);
   Subgraph_List_Menu : Gtk.Menu.Gtk_Menu;
   Subgraph_List : Gtk.Clist.Gtk_Clist;

   Status_Bar : Gtk.Status_Bar.Gtk_Status_Bar;

   ---------------------------------------------------------------------------
   --  Helper Methods
   ---------------------------------------------------------------------------

   function Get_Selected_Subgraph
     return String
   is
   begin
      return Subgraph_List_Data.Data.Get
        (Subgraph_List, Get_Selected_Row (Subgraph_List));
   end Get_Selected_Subgraph;

   function Get_Selected_Window
     return String
   is
   begin
      return Window_List_Data.Data.Get
        (Window_List, Get_Selected_Row (Window_List));
   end Get_Selected_Window;

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Boolean
   is
      Closed : Boolean;
   begin
      Closed := Controller.Hide_Gui;
      return True;
   end On_Delete;

   procedure Update_Window (Row : Glib.Gint)
   is
      Name: String := Window_List_Data.Data.Get (Window_List, Row);
   begin
      Gtk.Clist.Set_Text (Window_List, Row, 0, Name);
      if (Gui_Manager.Is_Window_Open (Name)) then
         Gtk.Clist.Set_Text (Window_List, Row, 1, -"Open");
      elsif (Projects.Is_Vis_Window_Memory_Loaded
             (Controller.Get_Project, Name)) then
         Gtk.Clist.Set_Text (Window_List, Row, 1, -"Loaded");
      else
         Gtk.Clist.Set_Text (Window_List, Row, 1, -"");
      end if;
   end Update_Window;

   ---------------------------------------------------------------------------
   --  Project Menu
   ---------------------------------------------------------------------------

   procedure On_Project_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"New Project", "",
            Dir_Only => False, Must_Exist => False);
      begin
         if (Filename /= "") then
            declare
               IML_Filename : String
                 := Gtkada.File_Selection.File_Selection_Dialog
                 (-"Select IML File", "",
                  Dir_Only => False, Must_Exist => False);
            begin
               if (Filename /= "") then
                  Controller.Create_Project
                    ("", Filename, IML_Filename);
               end if;
            end;
         end if;
      end;
   end On_Project_New;

   procedure On_Project_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"Open Project", "",
            Dir_Only => False, Must_Exist => True);
      begin
         if (Filename /= "") then
            Controller.Open_Project (Filename);
         end if;
      end;
   end On_Project_Open;

   procedure On_Project_Save
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Save_Project;
   end On_Project_Save;

   procedure On_Project_Save_As
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"Save Project", "",
            Dir_Only => False, Must_Exist => False);
      begin
         if (Filename /= "") then
            Controller.Save_Project (Filename);
         end if;
      end;
   end On_Project_Save_As;

   procedure On_Project_Quit
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Closed : Boolean;
   begin
      Closed := Controller.Hide_Gui;
   end On_Project_Quit;

   ---------------------------------------------------------------------------
   --  Tools Menu
   ---------------------------------------------------------------------------

   procedure On_Tools_Execute_GSL_Script
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog : Gsl_Dialog.Gsl_Dialog_Access;
   begin
      Gsl_Dialog.Create (Dialog);
      Gsl_Dialog.Show_All (Dialog);
   end On_Tools_Execute_GSL_Script;

   ---------------------------------------------------------------------------
   --  Window Menu
   ---------------------------------------------------------------------------

   procedure On_Window_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Create_Window;
   end On_Window_New;

   ---------------------------------------------------------------------------
   --  Window Context Menu
   ---------------------------------------------------------------------------

   procedure On_Window_List_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Open_Window (Get_Selected_Window);
   end On_Window_List_Open;

   procedure On_Window_List_Close
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Closed : Boolean;
   begin
      Closed := Controller.Close_Window (Get_Selected_Window);
   end On_Window_List_Close;

   procedure On_Window_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Remove_Window (Get_Selected_Window);
   end On_Window_List_Delete;

   ---------------------------------------------------------------------------
   --  Subgraph Menu
   ---------------------------------------------------------------------------

   procedure On_Subgraph_Set_Operation
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog : Set_Operation_Dialog.Set_Operation_Dialog_Access;
   begin
      Set_Operation_Dialog.Create (Dialog);
      Set_Operation_Dialog.Show_All (Dialog);
   end On_Subgraph_Set_Operation;

   ---------------------------------------------------------------------------
   --  Subgraph Context Menu
   ---------------------------------------------------------------------------

   procedure On_Subgraph_List_Highlight
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      --Controller.Remove_Window (Get_Selected_Window);
      null;
   end On_Subgraph_List_Highlight;

   procedure On_Subgraph_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      --Controller.Remove_Window (Get_Selected_Window);
      null;
   end On_Subgraph_List_Delete;

   ---------------------------------------------------------------------------
   --  Constructors
   ---------------------------------------------------------------------------

   function Initialize_Menu
     return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu : Gtk.Menu.Gtk_Menu;
   begin
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);

      Menu := New_Sub_Menu (Menu_Bar, -"Projekt");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Project_New'Access));
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Open...", On_Project_Open'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Save", On_Project_Save'Access));
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Save As...",
                                         On_Project_Save_As'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Quit", On_Project_Quit'Access));

      Menu := New_Sub_Menu (Menu_Bar, -"Tools");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Execute GSL Script..",
                                         On_Tools_Execute_GSL_Script'Access));

      Menu := New_Sub_Menu (Menu_Bar, -"Window");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Window_New'Access));

      Menu := New_Sub_Menu (Menu_Bar, -"Subgraph");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Set Operation",
                                         On_Subgraph_Set_Operation'Access));


      return Menu_Bar;
   end Initialize_Menu;

   procedure Initialize is
      Box : Gtk.Box.Gtk_Vbox;
      Pane : Gtk.Paned.Gtk_Vpaned;
   begin
      Gtk.Window.Initialize (Window, Window_Toplevel);
      Gtk.Window.Set_Title (Window, -"GIANT");

      Gtk.Box.Gtk_New_Vbox (Box);
      Gtk.Window.Add (Window, Box);

      --  menu
      Gtk.Box.Pack_Start (Box, Initialize_Menu, Expand => False, Fill => True,
                          Padding => 0);

      --  split pane
      Pane := New_Vpaned;
      Gtk.Box.Add (Box, Pane);

      --  window list popup menu
      Gtk.Menu.Gtk_New (Window_List_Menu);
      Gtk.Menu.Append (Window_List_Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Open", On_Window_List_Open'Access));
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Close", On_Window_List_Close'Access));
      Gtk.Menu.Append (Window_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Window_List_Delete'Access));

      --  window list
      Gtk.Clist.Gtk_New (Window_List, 2);
      Gtk.Clist.Set_Show_Titles (Window_List, True);
      Connect_Popup_Menu (Window_List, Window_List_Menu);

      Gtk.Clist.Set_Column_Title (Window_List, 0, -"Name");
      Gtk.Clist.Set_Column_Title (Window_List, 1, -"Status");

      Gtk.Paned.Add (Pane, Add_Scrollbar_And_Frame (Window_List, -"Windows"));

      --  sub graph list popup menu
      Gtk.Menu.Gtk_New (Subgraph_List_Menu);
      Gtk.Menu.Append (Subgraph_List_Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Highlight",
                                      On_Subgraph_List_Highlight'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Subgraph_List_Delete'Access));

      --  sub graph list
      Gtk.Clist.Gtk_New (Subgraph_List, 4);
      Gtk.Clist.Set_Show_Titles (Subgraph_List, True);
      Connect_Popup_Menu (Subgraph_List, Subgraph_List_Menu);

      Gtk.Clist.Set_Column_Title (Subgraph_List, 0, -"Name");
      Gtk.Clist.Set_Column_Title (Subgraph_List, 1, -"Nodes");
      Gtk.Clist.Set_Column_Title (Subgraph_List, 2, -"Edges");
      Gtk.Clist.Set_Column_Title (Subgraph_List, 3, -"Highlight Color");

      Gtk.Paned.Add (Pane, Add_Scrollbar_And_Frame (Subgraph_List, -"Subgraphs"));
      --  status bar
      Gtk.Status_Bar.Gtk_New (Status_Bar);
      Gtk.Box.Pack_End (Box, Status_Bar, Expand => False, Fill => True,
                        Padding => 0);

      --  connect close button
      Widget_Return_Callback.Connect
        (Window, "delete_event",
         Widget_Return_Callback.To_Marshaller (On_Delete'Access));
   end Initialize;

   ---------------------------------------------------------------------------
   --  Window Methods
   ---------------------------------------------------------------------------

   procedure Add_Window
     (Name : in String)
   is
      use Gtkada.Types;

      Row_Data : Gtkada.Types.Chars_Ptr_Array (0 .. 1);
      Row : Glib.Gint;
   begin
      for I in Row_Data'Range loop
         Row_Data (I) := Interfaces.C.Strings.New_String ("");
      end loop;
      Row := Gtk.Clist.Append (Window_List, Row_Data);
      Window_List_Data.Data.Set (Window_List, Row, Name);
      Update_Window (Row);
      Free (Row_Data);
   end Add_Window;

   procedure Update_Window
     (Name : in String)
   is
      Row : Glib.Gint
        := Window_List_Data.Find (Window_List, Name);
   begin
      if (Row /= -1) then
         Update_Window (Row);
      end if;
   end Update_Window;

   procedure Remove_Window
     (Name : in String)
   is
      Row : Glib.Gint
        := Window_List_Data.Find (Window_List, Name);
   begin
      if (Row /= -1) then
         Gtk.Clist.Remove (Window_List, Row);
      end if;
   end Remove_Window;

   ---------------------------------------------------------------------------
   --  Subgraph Methods
   ---------------------------------------------------------------------------



   ---------------------------------------------------------------------------
   --  Other Methods
   ---------------------------------------------------------------------------

   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean
   is
   begin
      -- FIX: ask user to save project
      return True;
   end Hide;

   procedure Show
   is
   begin
      Gtk.Window.Gtk_New (Window);
      Initialize;

      Gtk.Window.Show_All (Window);
   end Show;

end Giant.Main_Window;
