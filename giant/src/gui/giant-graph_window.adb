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
--  $RCSfile: giant-graph_window.adb,v $, $Revision: 1.14 $
--  $Author: squig $
--  $Date: 2003/06/24 10:43:05 $
--

with Glib;
with Gtk.Box;
with Gtk.Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Widget;

with Giant.Clists;
with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Dialogs;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Crosshair;
with Giant.Gui_Utils;
with Giant.Input_Dialog;

package body Giant.Graph_Window is

   package Graph_Window_Menu_Item_Callback is
    new Gtk.Handlers.User_Callback (Gtk.Menu_Item.Gtk_Menu_Item_Record,
                                    Graph_Window_Access);

   package Graph_Window_Input_Dialog is
     new Input_Dialog (Graph_Window_Access);

   procedure Update_Pin
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   procedure Update_Selection
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String);

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function Get_Selected_Selection
     (Window : access Graph_Window_Record'Class)
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Window.Selection_List);
   end Get_Selected_Selection;

   function Get_Selected_Pin
     (Window : access Graph_Window_Record)
     return String
   is
   begin
      return Gui_Utils.String_Clists.Get_Selected_Item (Window.Pin_List);
   end Get_Selected_Pin;

   function Get_Window_Name
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return String
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      return Vis_Windows.Get_Name (Window.Visual_Window);
   end Get_Window_Name;

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function On_Close
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Boolean
   is
      Closed : Boolean;
   begin
      Closed := Controller.Close_Window
        (Get_Window_Name (Gtk.Widget.Get_Toplevel (Source)));
      return True;
   end On_Close;

   procedure On_Pick_Edge_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      -- FIX: remove this:
      Gui_Manager.Crosshair.Trigger (Graph_Window_Access (Source));
      null;
   end On_Pick_Edge_Clicked;

   ---------------------------------------------------------------------------
   --  Pin Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Pin_List_Show
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Pin_List_Show;

   ---------------------------------------------------------------------------
   --  Selection Menu Callbacks
   ---------------------------------------------------------------------------

   function Validate_Selection_Name
     (Name   : in String;
      Window : in Graph_Window_Access)
      return Boolean
   is
   begin
      if (Vis_Windows.Does_Selection_Exist
          (Window.Visual_Window, Name)) then
         Dialogs.Show_Error_Dialog
           (-"A selection with this name already exists.");
         return False;
      end if;
      return True;
   end Validate_Selection_Name;

   procedure On_Selection_List_Duplicate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Source_Name : String := Get_Selected_Selection (Window);
   begin
      declare
         Target_Name : constant String
           := Graph_Window_Input_Dialog.Show
           (-"Target name", -"Duplicate Selection",
            Source_Name, Validate_Selection_Name'Access, Window);
      begin
         if (Target_Name /= "" and then Target_Name /= Source_Name) then
            Controller.Duplicate_Selection
              (Get_Window_Name (Source), Source_Name, Target_Name);
         end if;
      end;
   end On_Selection_List_Duplicate;

   procedure On_Selection_List_Hide
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Hide_Selection (Get_Window_Name (Source),
                                 Get_Selected_Selection (Window));
   end On_Selection_List_Hide;

   procedure On_Selection_List_Rename
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
   begin
      declare
         Old_Name : constant String := Get_Selected_Selection (Window);
         New_Name : constant String
           := Graph_Window_Input_Dialog.Show
           (-"New name", -"Rename Selection",
            Old_Name, Validate_Selection_Name'Access, Window);
      begin

         if (New_Name /= "" and then New_Name /= Old_Name) then
            Controller.Rename_Selection (Get_Window_Name (Source),
                                         Old_Name, New_Name);
         end if;
      end;
   end On_Selection_List_Rename;

   procedure On_Selection_List_Delete
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
      Removed : Boolean;
   begin
      Removed := Controller.Remove_Selection
        (Get_Window_Name (Source),
         Get_Selected_Selection (Window));
   end On_Selection_List_Delete;

   procedure On_Selection_List_Show
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Show_Selection (Get_Window_Name (Source),
                                 Get_Selected_Selection (Window));
   end On_Selection_List_Show;

   procedure On_Selection_List_Show_All
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Show_All_Selections
        (Vis_Windows.Get_Name (Window.Visual_Window));
   end On_Selection_List_Show_All;


   ---------------------------------------------------------------------------
   --  Zoom Callbacks
   ---------------------------------------------------------------------------

   procedure On_Zoom_In_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Zoom_In_Clicked;

   procedure On_Zoom_Out_Clicked
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Zoom_Out_Clicked;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Window        :    out Graph_Window_Access;
      Visual_Window : in     Vis_Windows.Visual_Window_Access)
   is
   begin
      Window := new Graph_Window_Record;
      Window.Visual_Window := Visual_Window;
      Initialize (Window);
   end Create;

   procedure Initialize
     (Window : access Graph_Window_Record'Class)
   is
      use Giant.Gui_Utils;

      Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Left_Box : Gtk.Box.Gtk_Vbox;
      Left_Paned : Gtk.Paned.Gtk_Paned;
      Vbox : Gtk.Box.Gtk_Vbox;
      Hbox : Gtk.Box.Gtk_Hbox;
      Zoom_Levels : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Window.Initialize (Window, Gtk.Enums.Window_Toplevel);
      Update_Title (Window);

      --  horizontal split pane
      Gtk.Paned.Gtk_New_Hpaned (Window.Split_Pane);
      Gtk.Paned.Set_Handle_Size (Window.Split_Pane, 8);
      Gtk.Paned.Set_Gutter_Size (Window.Split_Pane, 12);
      Add (Window, Window.Split_Pane);

      --  left box
      Gtk.Box.Gtk_New_Vbox (Left_Box, Homogeneous => False,
                            Spacing => DEFAULT_SPACING);
      Gtk.Box.Set_Border_Width (Left_Box, DEFAULT_SPACING);
      Gtk.Paned.Pack1 (Window.Split_Pane, Left_Box,
                       Resize => False, Shrink => False);

      -- minimap
      Gtk.Box.Pack_Start (Left_Box,
                          Add_Frame (New_Label ("[Missing]"), -"MiniMap"),
                          Expand => False, Fill => True, Padding => 0);

      --  vertical split pane
      Gtk.Paned.Gtk_New_Vpaned (Left_Paned);
      Gtk.Paned.Set_Handle_Size (Left_Paned, 8);
      Gtk.Paned.Set_Gutter_Size (Left_Paned, 12);
      Gtk.Box.Pack_Start (Left_Box, Left_Paned,
                          Expand => True, Fill => True, Padding => 0);

      --  pins list menu
      Gtk.Menu.Gtk_New (Window.Pin_List_Menu);
      Gtk.Menu.Append (Window.Pin_List_Menu,
                       New_Menu_Item (-"Show",
                                      On_Pin_List_Show'Access,
                                      Window));

      --  pins
      Gui_Utils.String_Clists.Create (Window.Pin_List, 1, Update_Pin'Access);
      Gui_Utils.String_Clists.Connect_Popup_Menu
        (Window.Pin_List, Window.Pin_List_Menu);

      Gui_Utils.String_Clists.Set_Column_Title (Window.Pin_List, 0, -"Pin");

      Gtk.Paned.Add1 (Left_Paned, Add_Scrollbars (Window.Pin_List));

      --  selections list menu
      Gtk.Menu.Gtk_New (Window.Selection_List_Menu);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Show", On_Selection_List_Show'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Hide", On_Selection_List_Hide'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Show All",
                                      On_Selection_List_Show_All'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Rename...",
                                      On_Selection_List_Rename'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Dupliate...",
                                      On_Selection_List_Duplicate'Access,
                                      Window));
      Gtk.Menu.Append (Window.Selection_List_Menu, New_Menu_Separator);
      Gtk.Menu.Append (Window.Selection_List_Menu,
                       New_Menu_Item (-"Delete",
                                      On_Selection_List_Delete'Access,
                                      Window));

      --  selections
      Gui_Utils.String_Clists.Create (Window.Selection_List, 2,
                                      Update_Selection'Access);
      Gui_Utils.String_Clists.Set_Show_Titles (Window.Selection_List, True);
      Gui_Utils.String_Clists.Connect_Popup_Menu
        (Window.Selection_List, Window.Selection_List_Menu);

      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 0, -"Selection");
      Gui_Utils.String_Clists.Set_Column_Title
        (Window.Selection_List, 1, -"Color");

      Gtk.Paned.Add2 (Left_Paned,
                      Add_Scrollbars (Window.Selection_List));

      --  visualization style
      Gtk.Option_Menu.Gtk_New (Window.Vis_Style_Menu);
      Gtk.Option_Menu.Set_Border_Width (Window.Vis_Style_Menu,
                                        DEFAULT_SPACING);
      Gtk.Menu.Gtk_New (Menu);
      Gtk.Menu_Item.Gtk_New (Menu_Item, -"Default");
      Gtk.Menu_Item.Show (Menu_Item);
      Gtk.Menu.Append (Menu, Menu_Item);
      Gtk.Menu_Item.Gtk_New (Menu_Item, -"Fancy");
      Gtk.Menu.Append (Menu, Menu_Item);
      Gtk.Menu_Item.Gtk_New (Menu_Item, -"Other");
      Gtk.Menu.Append (Menu, Menu_Item);
      Gtk.Option_Menu.Set_Menu(Window.Vis_Style_Menu, Menu);
      Gtk.Box.Pack_Start (Left_Box,
                          Add_Frame (Window.Vis_Style_Menu, -"Style"),
                          Expand => False, Fill => False, Padding => 0);

      --  zoom
      Gtk.Box.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Gtk.Box.Set_Border_Width (Vbox, DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Left_Box, Add_Frame (Vbox, -"Zoom"),
                          Expand => False, Fill => False, Padding => 0);

      --  zoom selection
      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
      Gtk.Box.Pack_Start (Vbox, Hbox,
                          Expand => False, Fill => False, Padding => 0);

      Gtk.Box.Pack_Start (Hbox,
                          New_Button (" - ", On_Zoom_Out_Clicked'Access,
                                      Window),
                          Expand => False, Fill => False, Padding => 0);

      Gtk.Enums.String_List.Append (Zoom_Levels, -"100%");
      Gtk.Enums.String_List.Append (Zoom_Levels, -"50%");
      Gtk.Enums.String_List.Append (Zoom_Levels, -"Whole Graph");

      Gtk.Combo.Gtk_New (Window.Zoom_Combo);
      Gtk.Combo.Set_Popdown_Strings (Window.Zoom_Combo, Zoom_Levels);
      Gtk.Box.Pack_Start (Hbox, Window.Zoom_Combo,
                          Expand => False, Fill => False, Padding => 0);
      Gtk.Combo.Set_Usize (Window.Zoom_Combo, 100, Glib.Gint (-1));

      Window.Zoom_Entry := Gtk.Combo.Get_Entry (Window.Zoom_Combo);

      Gtk.Box.Pack_Start (Hbox,
                          New_Button (" + ", On_Zoom_In_Clicked'Access,
                                      Window),
                          Expand => False, Fill => False, Padding => 0);

      --  pick edge
      Gtk.Box.Gtk_New_Hbox (Hbox, Homogeneous => False,
                            Spacing => DEFAULT_SPACING);
      Gtk.Box.Pack_Start (Vbox, Hbox,
                          Expand => False, Fill => False, Padding => 0);

      Gtk.Box.Pack_Start (Hbox,
                          New_Button (-"Pick Edge",
                                      On_Pick_Edge_Clicked'Access, Window),
                          Expand => False, Fill => False, Padding => 0);


      --  graph widget
      Gtk.Paned.Pack2 (Window.Split_Pane, New_Label ("Graph Widget"),
                       Resize => True, Shrink => False);

      -- listen for the close button
      Widget_Return_Callback.Connect
        (Window, "delete_event",
         Widget_Return_Callback.To_Marshaller (On_Close'Access));
   end;

   ---------------------------------------------------------------------------
   --  Public Methods
   ---------------------------------------------------------------------------

   function Close
     (Window : access Graph_Window_Record'Class)
     return Boolean
   is
      use type Default_Dialog.Response_Type;

      Response : Default_Dialog.Response_Type;
   begin
      if (Window.Is_Dirty) then
         Response := Dialogs.Show_Confirmation_Dialog
           (-"The content has changed. Save changes?",
            Default_Dialog.Button_Yes_No_Cancel);
         if (Response = Default_Dialog.Response_Yes) then
            -- FIX: save changes
            null;
         elsif (Response = Default_Dialog.Response_Cancel) then
            return False;
         end if;
      end if;

      Hide (Window);
      return True;
   end;

   function Get_Vis_Window
     (Window : access Graph_Window_Record'Class)
     return Vis_Windows.Visual_Window_Access
   is
   begin
      return Window.Visual_Window;
   end Get_Vis_Window;

   procedure Update_Title
     (Window : access Graph_Window_Record'Class)
   is
   begin
      Set_Title (Window, Vis_Windows.Get_Name (Window.Visual_Window));
   end Update_Title;

   procedure Set_Crosshair_Mode
     (Widget : access Graph_Window_Record'Class;
      Enable : in     Boolean)
   is
   begin
      null;
   end Set_Crosshair_Mode;

   ---------------------------------------------------------------------------
   --  Pin Methods
   ---------------------------------------------------------------------------

   procedure Update_Pin
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
   end Update_Pin;

   procedure Add_Pin
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Add (Window.Pin_List, Name);
   end Add_Pin;

   procedure Remove_Pin
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Window.Pin_List, Name);
   end Remove_Pin;

   procedure Update_Pin
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Update (Window.Pin_List, Name);
   end Update_Pin;

   ---------------------------------------------------------------------------
   --  Selection Methods
   ---------------------------------------------------------------------------

   procedure Update_Selection
     (List : access Gui_Utils.String_Clists.Giant_Data_Clist_Record;
      Row  : in     Glib.Gint;
      Name : in     String)
   is
   begin
      Gui_Utils.String_Clists.Set_Text (List, Row, 0, Name);
   end Update_Selection;

   procedure Add_Selection
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Add (Window.Selection_List, Name);
   end Add_Selection;

   procedure Remove_Selection
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Remove (Window.Selection_List, Name);
   end Remove_Selection;

   procedure Update_Selection
     (Window : access Graph_Window_Record'Class;
      Name   : in     String)
   is
   begin
      Gui_Utils.String_Clists.Update (Window.Selection_List, Name);
   end Update_Selection;

end Giant.Graph_Window;
