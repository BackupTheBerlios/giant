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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003/06/19 19:37:06 $
--

with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

with Gdk.Event;
with Gdk.Types;
with Glib; use type Glib.Gint;
with Gtk.Box;
with Gtk.Clist;
pragma Elaborate_All (Gtk.Clist);
with Gtk.Container;
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

with Giant.Clists;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Gsl_Dialog;
with Giant.Gui_Manager;
with Giant.Gui_Utils; use Giant.Gui_Utils;
with Giant.Projects;
with Giant.Set_Operation_Dialog;

package body Giant.Main_Window is

   procedure Update_Subgraph
     (List : access String_Clists.Giant_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   procedure Update_Window
     (List : access String_Clists.Giant_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   --  main window instance
   Window : Gtk.Window.Gtk_Window;

   Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
   Project_Menu : Gtk.Menu.Gtk_Menu;
   Project_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_New_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Open_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Quit_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

   Window_List_Menu : Gtk.Menu.Gtk_Menu;
   Window_List : String_Clists.Giant_Clist;

   Subgraph_List_Menu : Gtk.Menu.Gtk_Menu;
   Subgraph_List : String_Clists.Giant_Clist;

   Status_Bar : Gtk.Status_Bar.Gtk_Status_Bar;

   ---------------------------------------------------------------------------
   --  Helper Methods
   ---------------------------------------------------------------------------

   function Get_Selected_Subgraph
     return String
   is
   begin
      return String_Clists.Get_Selected_Item (Subgraph_List);
   end Get_Selected_Subgraph;

   function Get_Selected_Window
     return String
   is
   begin
      return String_Clists.Get_Selected_Item (Window_List);
   end Get_Selected_Window;

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function On_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Boolean
   is
      Closed : Boolean;
   begin
      Closed := Controller.Hide_Gui;
      return True;
   end On_Delete;

   ---------------------------------------------------------------------------
   --  Project Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Project_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         --FIX: default directory
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"New Project", "test/resources/",
            Dir_Only => False, Must_Exist => False);
      begin
         if (Filename /= "") then
            declare
               IML_Filename : String
                 := Gtkada.File_Selection.File_Selection_Dialog
                 (-"Select IML File", Filename & ".iml",
                  Dir_Only => False, Must_Exist => False);
            begin
               if (IML_Filename /= "") then
                  Controller.Create_Project (Filename, IML_Filename);
               end if;
            end;
         end if;
      end;
   exception
      when Projects.Directory_Holds_Already_A_Project_File_Exception =>
         Default_Dialog.Show_Error_Dialog (-"The project could not be created. The directory already contains a project.");
   end On_Project_New;

   procedure On_Project_Open
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      declare
         --FIX: default directory
         Filename : String := Gtkada.File_Selection.File_Selection_Dialog
           (-"Open Project", "test/resources/",
            Dir_Only => False, Must_Exist => True);
      begin
         if (Filename /= "") then
            Controller.Open_Project (Filename);
         end if;
      end;
   exception
      when Projects.Project_Does_Not_Exist_Exception =>
         Default_Dialog.Show_Error_Dialog (-"The project could not be opened. One or more project files are missing.");
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
   --  Tools Menu Callbacks
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
   --  Window Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Window_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Create_Window;
   end On_Window_New;

   ---------------------------------------------------------------------------
   --  Window Context Menu Callbacks
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

   procedure On_Window_List_Rename
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Old_Name : String := Get_Selected_Window;
   begin
      declare
         New_Name : constant String
           := Default_Dialog.Show_Input_Dialog (-"New name", -"Rename Window",
                                                Old_Name);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Window (Old_Name, New_Name);
         end if;
      end;
   end On_Window_List_Rename;

   procedure On_Window_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Window (Get_Selected_Window);
   end On_Window_List_Delete;

   ---------------------------------------------------------------------------
   --  Subgraph Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Subgraph_New
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      Controller.Create_Subgraph;
   end On_Subgraph_New;

   procedure On_Subgraph_Set_Operation
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog : Set_Operation_Dialog.Set_Operation_Dialog_Access;
   begin
      Set_Operation_Dialog.Create (Dialog);
      Set_Operation_Dialog.Show_All (Dialog);
   end On_Subgraph_Set_Operation;

   ---------------------------------------------------------------------------
   --  Subgraph Context Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Subgraph_List_Highlight
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      --Controller.Remove_Subgraph (Get_Selected_Subgraph);
      null;
   end On_Subgraph_List_Highlight;

   procedure On_Subgraph_List_Delete
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Subgraph (Get_Selected_Subgraph);
   end On_Subgraph_List_Delete;

   procedure On_Subgraph_List_Rename
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Old_Name : String := Get_Selected_Subgraph;
   begin
      declare
         New_Name : constant String
           := Default_Dialog.Show_Input_Dialog (-"New name",
                                                -"Rename Subgraph",
                                                Old_Name);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Subgraph (Old_Name, New_Name);
         end if;
      end;
   end On_Subgraph_List_Rename;

   ---------------------------------------------------------------------------
   --  Status Bar Callbacks
   ---------------------------------------------------------------------------

   procedure On_Log_Message
     (Level   : in Default_Logger.Level_Type;
      Name    : in String;
      Message : in String)
   is
      use type Default_Logger.Level_Type;
      Id : Gtk.Status_Bar.Message_Id;
   begin
      if (Level = Default_Logger.Level_Info) then
         Id := Gtk.Status_Bar.Push (Status_Bar, 1, Message);
      end if;
   end;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   function Initialize_Menu
     return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Menu : Gtk.Menu.Gtk_Menu;
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Gtk.Menu_Bar.Gtk_New (Menu_Bar);

      Gtk.Menu_item.Gtk_New (Item, -"Project");
      Gtk.Menu_Bar.Add (Menu_Bar, Item);
      Project_Menu_Item := Item;

      Gtk.Menu.Gtk_New (Menu);
      Gtk.Menu_Item.Set_Submenu (Item, Menu);
      Project_Menu := Menu;

      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Project_New_Menu_Item := New_Menu_Item (-"New", On_Project_New'Access);
      Gtk.Menu.Add (Menu, Project_New_Menu_Item);
      Project_Open_Menu_Item
        := New_Menu_Item (-"Open...", On_Project_Open'Access);
      Gtk.Menu.Add (Menu, Project_Open_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Save", On_Project_Save'Access));
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Save As...",
                                         On_Project_Save_As'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Project_Quit_Menu_Item := New_Menu_Item (-"Quit", On_Project_Quit'Access);
      Gtk.Menu.Add (Menu, Project_Quit_Menu_Item);

      Menu := New_Sub_Menu (Menu_Bar, -"Tools");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Execute GSL Script..",
                                         On_Tools_Execute_GSL_Script'Access));

      Menu := New_Sub_Menu (Menu_Bar, -"Window");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Window_New'Access));

      Menu := New_Sub_Menu (Menu_Bar, -"Subgraph");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"New", On_Subgraph_New'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Set Operation...",
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

      --  menu bar
      Menu_Bar := Initialize_Menu;
      Gtk.Box.Pack_Start (Box, Menu_Bar, Expand => False, Fill => True,
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
                       New_Menu_Item (-"Rename...",
                                      On_Window_List_Rename'Access));
      Gtk.Menu.Append (Window_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Window_List_Delete'Access));

      --  window list
      String_Clists.Create (Window_List, 2, Update_Window'Access);
      Connect_Popup_Menu (Window_List, Window_List_Menu);

      String_Clists.Set_Column_Title (Window_List, 0, -"Name");
      String_Clists.Set_Column_Title (Window_List, 1, -"Status");

      Gtk.Paned.Add (Pane, Add_Scrollbar_And_Frame (Window_List, -"Windows"));

      --  sub graph list popup menu
      Gtk.Menu.Gtk_New (Subgraph_List_Menu);
      Gtk.Menu.Append (Subgraph_List_Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Highlight",
                                      On_Subgraph_List_Highlight'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Subgraph_List_Rename'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Subgraph_List_Delete'Access));

      --  sub graph list
      String_Clists.Create (Subgraph_List, 4, Update_Subgraph'Access);
      String_Clists.Set_Show_Titles (Subgraph_List, True);
      Connect_Popup_Menu (Subgraph_List, Subgraph_List_Menu);

      String_Clists.Set_Column_Title (Subgraph_List, 0, -"Name");
      String_Clists.Set_Column_Title (Subgraph_List, 1, -"Nodes");
      String_Clists.Set_Column_Title (Subgraph_List, 2, -"Edges");
      String_Clists.Set_Column_Title (Subgraph_List, 3, -"Highlight Color");

      Gtk.Paned.Add (Pane, Add_Scrollbar_And_Frame (Subgraph_List, -"Subgraphs"));
      --  status bar
      Gtk.Status_Bar.Gtk_New (Status_Bar);
      Gtk.Box.Pack_End (Box, Status_Bar, Expand => False, Fill => True,
                        Padding => 0);
      Default_Logger.Set_Listener (On_Log_Message'Access);

      --  connect close button
      Widget_Return_Callback.Connect
        (Window, "delete_event",
         Widget_Return_Callback.To_Marshaller (On_Delete'Access));
   end Initialize;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   procedure Update_Window
     (List : access String_Clists.Giant_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      String_Clists.Set_Text (List, Row, 0, Name);
      if (Gui_Manager.Is_Window_Open (Name)) then
         String_Clists.Set_Text (List, Row, 1, -"Open");
      elsif (Projects.Is_Vis_Window_Memory_Loaded
             (Controller.Get_Project, Name)) then
         String_Clists.Set_Text (List, Row, 1, -"Loaded");
      else
         String_Clists.Set_Text (List, Row, 1, -"");
      end if;
   end Update_Window;

   procedure Add_Window
     (Name : in String)
   is
   begin
      String_Clists.Add (Window_List, Name);
   end Add_Window;

   procedure Update_Window
     (Name : in String)
   is
   begin
      String_Clists.Update (Window_List, Name);
   end Update_Window;

   procedure Remove_Window
     (Name : in String)
   is
   begin
      String_Clists.Remove (Window_List, Name);
   end Remove_Window;

   ---------------------------------------------------------------------------
   --  Subgraph Methods
   ---------------------------------------------------------------------------

   procedure Update_Subgraph
     (List : access String_Clists.Giant_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      String_Clists.Set_Text (List, Row, 0, Name);
      -- FIX: fill in missing values
      String_Clists.Set_Text (List, Row, 1, "FIX");
      String_Clists.Set_Text (List, Row, 2, "FIX");
      String_Clists.Set_Text (List, Row, 3, "FIX: Ask Martin");
   end Update_Subgraph;

   procedure Add_Subgraph
     (Name : in String)
   is
   begin
      String_Clists.Add (Subgraph_List, Name);
   end Add_Subgraph;

   procedure Update_Subgraph
     (Name : in String)
   is
   begin
      String_Clists.Update (Subgraph_List, Name);
   end Update_Subgraph;

   procedure Remove_Subgraph
     (Name : in String)
   is
   begin
      String_Clists.Remove (Subgraph_List, Name);
   end Remove_Subgraph;

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

   Loaded: Boolean;

   procedure Update_Children
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is

   begin
      Gtk.Widget.Set_Sensitive (Widget, Loaded);
   end;

   procedure Set_Project_Loaded
     (Loaded : in Boolean)
   is
--        package Menu_Bar_Forall is new Gtk.Container.Forall_Pkg
--          (Boolean);

   begin
      --  set menu bar sensitive
      Main_Window.Loaded := Loaded;
      Gtk.Menu_Bar.Forall (Menu_Bar, Update_Children'Access);
      Gtk.Menu.Forall (Project_Menu, Update_Children'Access);

      if (Loaded) then
         Gtk.Window.Set_Title (Window, "GIANT - "
                               & Projects.Get_Project_Name
                               (Controller.Get_Project));
      else
         Gtk.Window.Set_Title (Window, "GIANT");

         --  active a few items that can be selected when no project
         --  is loaded
         Gtk.Menu_Item.Set_Sensitive (Project_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_New_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_Open_Menu_Item, True);
         Gtk.Menu_Item.Set_Sensitive (Project_Quit_Menu_Item, True);
      end if;
   end Set_Project_Loaded;

end Giant.Main_Window;
