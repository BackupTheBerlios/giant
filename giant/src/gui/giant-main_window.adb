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
--  $RCSfile: giant-main_window.adb,v $, $Revision: 1.27 $
--  $Author: squig $
--  $Date: 2003/06/23 21:57:04 $
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
with Gtk.Object;
with Gtk.Paned;
with Gtk.Status_Bar;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Tearoff_Menu_Item;
with Gtkada.File_Selection;
with Gtkada.Types;

with Giant.About_Dialog;
with Giant.Clists;
with Giant.Config_Settings;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Default_Logger;
with Giant.Dialogs;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gsl_Dialog;
with Giant.Gui_Manager;
with Giant.Gui_Utils; use Giant.Gui_Utils;
with Giant.Logger;
with Giant.Node_Info_Dialog;
with Giant.Projects;
with Giant.Set_Operation_Dialog;

package body Giant.Main_Window is

   package Logger is new Giant.Logger("giant.main_window");

   procedure Update_Subgraph
     (List : access String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   procedure Update_Window
     (List : access String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   --  main window instance
   Window : Gtk.Window.Gtk_Window;

   Pane : Gtk.Paned.Gtk_Vpaned;

   Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
   Project_Menu : Gtk.Menu.Gtk_Menu;
   Project_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_New_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Open_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   Project_Quit_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

   Window_List_Menu : Gtk.Menu.Gtk_Menu;
   Window_List : String_Clists.Giant_Data_Clist;

   Subgraph_List_Menu : Gtk.Menu.Gtk_Menu;
   Subgraph_List : String_Clists.Giant_Data_Clist;

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
         Dialogs.Show_Error_Dialog (-"The project could not be created. The directory already contains a project.");
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
         Dialogs.Show_Error_Dialog (-"The project could not be opened. One or more project files are missing.");
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

   procedure On_Project_Info
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Dialog: Node_Info_Dialog.Node_Info_Dialog_Access;
   begin
      Logger.Info ("Nodes : " & Integer'Image
                   (Graph_Lib.Node_Id_Sets.Size
                    (Graph_Lib.Get_All_Nodes)));
      Node_Info_Dialog.Create (Dialog);
      Node_Info_Dialog.Set_Node (Dialog, Graph_Lib.Get_Root_Node);
      Node_Info_Dialog.Show_All (Dialog);
   end On_Project_Info;

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
   --  Help Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Help_About
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      About_Dialog.Show;
   end On_Help_About;

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

   function Validate_Window_Name
     (Name   : in String;
      Widget : in Gtk.Object.Gtk_Object)
      return Boolean
   is
   begin
      if (Projects.Does_Vis_Window_Exist (Controller.Get_Project, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A window with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Window_Name;

   procedure On_Window_List_Rename
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is

      Old_Name : String := Get_Selected_Window;
   begin
      declare
         New_Name : constant String
           := Dialogs.Show_Input_Dialog
           (-"New name", -"Rename Window",
            Old_Name, Validate_Window_Name'Access);
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

   function Validate_Subgraph_Name
     (Name   : in String;
      Widget : in Gtk.Object.Gtk_Object)
      return Boolean
   is
   begin
      if (Projects.Does_Subgraph_Exist (Controller.Get_Project, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A subgraph with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Subgraph_Name;

   procedure On_Subgraph_List_Highlight
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      --Controller.Remove_Subgraph (Get_Selected_Subgraph);
      null;
   end On_Subgraph_List_Highlight;

   procedure On_Subgraph_List_Unhightlight
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
   begin
      --Controller.Remove_Subgraph (Get_Selected_Subgraph);
      null;
   end On_Subgraph_List_Unhightlight;

   procedure On_Subgraph_List_Create_Selection
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      Subgraph_Name : String := Get_Selected_Subgraph;
      Action : Create_Selection_Action_Access;
   begin
      Action := new
        Create_Selection_Action_Type(Subgraph_Name'Length);
      Action.Subgraph_Name := Subgraph_Name;
      Gui_Manager.Crosshair.Enqueue (Action);
   end On_Subgraph_List_Create_Selection;

   procedure On_Subgraph_List_Duplicate
     (Source : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
     Source_Name : String := Get_Selected_Subgraph;
   begin
      declare
         Target_Name : constant String
           := Dialogs.Show_Input_Dialog
           (-"Target name", -"Duplicate Subgraph",
            Source_Name, Validate_Subgraph_Name'Access);
      begin
         if (Target_Name /= "" and then Target_Name /= Source_Name) then
            Controller.Duplicate_Subgraph (Source_Name, Target_Name);
         end if;
      end;
   end On_Subgraph_List_Duplicate;

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
           := Dialogs.Show_Input_Dialog
           (-"New name", -"Rename Subgraph",
            Old_Name, Validate_Subgraph_Name'Access);
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
   begin
      if (Level = Default_Logger.Level_Info) then
         Set_Status (Message);
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
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Info...",
                                         On_Project_Info'Access));
      Gtk.Menu.Add (Menu, New_Menu_Separator);
      Project_Quit_Menu_Item := New_Menu_Item (-"Quit", On_Project_Quit'Access);
      Gtk.Menu.Add (Menu, Project_Quit_Menu_Item);

      Menu := New_Sub_Menu (Menu_Bar, -"Tools");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"Execute GSL Script...",
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

      Menu := New_Sub_Menu (Menu_Bar, -"Help");
      Gtk.Menu.Add (Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Add (Menu, New_Menu_Item (-"About...", On_Help_About'Access));

      return Menu_Bar;
   end Initialize_Menu;

   procedure Initialize is
      Box : Gtk.Box.Gtk_Vbox;
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
      String_Clists.Connect_Popup_Menu (Window_List, Window_List_Menu);

      String_Clists.Set_Column_Title (Window_List, 0, -"Window");
      String_Clists.Set_Column_Title (Window_List, 1, -"Status");

      Gtk.Paned.Add (Pane, Add_Scrollbars (Window_List));

      --  sub graph list popup menu
      Gtk.Menu.Gtk_New (Subgraph_List_Menu);
      Gtk.Menu.Append (Subgraph_List_Menu, New_TearOff_Menu_Item);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Highlight",
                                      On_Subgraph_List_Highlight'Access));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Unhighlight In All Windows",
                                      On_Subgraph_List_Unhightlight'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Insert As Selection...",
                                      On_Subgraph_List_Create_Selection'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Subgraph_List_Rename'Access));
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Duplicate...",
                                      On_Subgraph_List_Duplicate'Access));
      Gtk.Menu.Append (Subgraph_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Subgraph_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Subgraph_List_Delete'Access));

      --  sub graph list
      String_Clists.Create (Subgraph_List, 4, Update_Subgraph'Access);
      String_Clists.Set_Show_Titles (Subgraph_List, True);
      String_Clists.Connect_Popup_Menu (Subgraph_List, Subgraph_List_Menu);

      String_Clists.Set_Column_Title (Subgraph_List, 0, -"Subgraph");
      String_Clists.Set_Column_Title (Subgraph_List, 1, -"Nodes");
      String_Clists.Set_Column_Title (Subgraph_List, 2, -"Edges");
      String_Clists.Set_Column_Title (Subgraph_List, 3, -"Highlight Color");

      Gtk.Paned.Add (Pane, Add_Scrollbars (Subgraph_List));
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
     (List : access String_Clists.Giant_Data_Clist_Record;
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
     (List : access String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Projects.Get_Subgraph (Controller.Get_Project, Name);
   begin
      String_Clists.Set_Text (List, Row, 0, Name);
      String_Clists.Set_Text (List, Row, 1,
                              Natural'Image (Graph_Lib.Subgraphs.Get_Node_Count (Subgraph)));
      String_Clists.Set_Text (List, Row, 2,
                              Natural'Image (Graph_Lib.Subgraphs.Get_Edge_Count (Subgraph)));
      -- FIX: fill in missing values
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
   --  Public Methods
   ---------------------------------------------------------------------------

   function Hide
     (Ask_For_Confirmation: Boolean)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      Response : Default_Dialog.Response_Type;
   begin
      Response := Dialogs.Show_Confirmation_Dialog
        (-"The project has changed. Save changes?",
         Default_Dialog.Button_Yes_No_Cancel);
      if (Response = Default_Dialog.Response_Yes) then
         -- FIX: save changes
         Controller.Save_Project;
      elsif (Response = Default_Dialog.Response_Cancel) then
         return False;
      end if;

      --  save settings
      Config_Settings.Set_Setting
        ("Main_Window.Width",
         Integer (Gtk.Window.Get_Allocation_Width (Window)));
      Config_Settings.Set_Setting
        ("Main_Window.Height",
         Integer (Gtk.Window.Get_Allocation_Height (Window)));
      Config_Settings.Set_Setting
        ("Main_Window.Separator",
         Integer (String_Clists.Get_Allocation_Height (Window_List)));

      return True;
   end Hide;

   procedure Show
   is
   begin
      Gtk.Window.Gtk_New (Window);
      Initialize;

      --  restore size
      Gtk.Window.Set_Default_Size
        (Window,
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Width")),
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Height")));
      Gtk.Paned.Set_Position
        (Pane,
         Glib.Gint (Config_Settings.Get_Setting_As_Integer
                    ("Main_Window.Separator")));

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

   procedure Set_Status
     (Text : in String)
   is
      Id : Gtk.Status_Bar.Message_Id;
   begin
      Id := Gtk.Status_Bar.Push (Status_Bar, 1, Text);
   end Set_Status;

   ---------------------------------------------------------------------------
   --  Subgraph Crosshair
   ---------------------------------------------------------------------------

   procedure Cancel
     (Action : access Create_Selection_Action_Type)
   is
   begin
      Destroy (Action);
   end;

   procedure Execute
     (Action : access Create_Selection_Action_Type;
      Window : access Graph_Window.Graph_Window_Record'Class)
   is
   begin
      Controller.Create_Selection_From_Subgraph
        (Action.Subgraph_Name,
         Vis_Windows.Get_Name (Graph_Window.Get_Vis_Window (Window)),
         Action.Subgraph_Name);
   end;

end Giant.Main_Window;
