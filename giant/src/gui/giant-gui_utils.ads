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
--  $RCSfile: giant-gui_utils.ads,v $, $Revision: 1.22 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
--  Provides common gui utility methods.
--

with Glib;
with Gtk.Button;
with Gtk.Clist;
with Gtk.Editable;
With Gtk.Frame;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Shell;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Misc;
with Gtk.Paned;
with Gtk.Pixmap;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Table;
with Gtk.Widget;
with Gtk.Window;

with Giant.Data_Clists;
pragma Elaborate_All (Giant.Data_Clists);
with Giant.Projects;
with Giant.Vis_Windows;

package Giant.Gui_Utils is

   -----------------
   --  Constants  --
   -----------------

   ADA_DEFAULT_SPACING : constant Integer := 5;
   ADA_BUTTON_SPACING : constant Integer := 10;

   function Default_Spacing return Glib.Guint;
   function Default_Spacing return Glib.Gint;
   function Button_Spacing return Glib.Guint;
   function Button_Spacing return Glib.Gint;

   -----------------
   --  Callbacks  --
   -----------------

   package Button_Callback is new
     Gtk.Handlers.Callback (Gtk.Button.Gtk_Button_Record);

   package Clist_Callback is new
     Gtk.Handlers.Callback (Gtk.Clist.Gtk_Clist_Record);

   package Clist_Boolean_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Clist.Gtk_Clist_Record, Boolean);

   package Editable_Callback is new
     Gtk.Handlers.Callback (Gtk.Editable.Gtk_Editable_Record);

   package Menu_Item_Callback is new
     Gtk.Handlers.Callback (Gtk.Menu_Item.Gtk_Menu_Item_Record);

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk.Widget.Gtk_Widget_Record);

   package Widget_Boolean_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Widget.Gtk_Widget_Record, Boolean);

   ---------------------------------------------------------------------------
   --  Generic Packages
   ---------------------------------------------------------------------------

   package String_Clists is new Giant.Data_Clists (String);

   ---------------
   --  Methods  --
   ---------------

   function Add_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'class;
      Title  : in     String)
     return Gtk.Frame.Gtk_Frame;

   ---------------------------------------------------------------------------
   --  Adds a label and widget to a table. The label is right aligned.
   --
   --  The table must have at least two columns.
   --
   --  Parameters:
   --    Table - The table
   --    Left - The label
   --    Right - The widget
   --    Row - The row, is increased by 1
   --
   procedure Add_Row
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in out Glib.Guint;
      Left  : access Gtk.Misc.Gtk_Misc_Record'Class;
      Right : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Adds two labels to a table. Both labels are left aligned.
   --
   --  The table must have at least two columns.
   --
   --  Parameters:
   --    Table - The table
   --    Left - The left label
   --    Right - The right label
   --    Row - The row, is increased by 1
   --  See:
   --    Add_Row
   procedure Add_Row_Labels
     (Table : in     Gtk.Table.Gtk_Table;
      Row   : in out Glib.Guint;
      Left  : access Gtk.Misc.Gtk_Misc_Record'Class;
      Right : access Gtk.Misc.Gtk_Misc_Record'Class);

   function Add_Scrollbars
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Scrolled_Window.Gtk_Scrolled_Window;

   function Add_Scrollbars_And_Frame
     (Widget : access Gtk.Widget.Gtk_Widget_Record'class;
      Title  : in String)
     return Gtk.Frame.Gtk_Frame;

   ---------------------------------------------------------------------------
   --  Returns an absolute path to the passed icon.
   function Get_Icon
     (Filename : in String)
     return String;

   function Get_Selected_Row
     (List : access Gtk.Clist.Gtk_Clist_Record'Class)
     return Glib.Gint;

   function New_Button
     (Label      : in String;
      Callback   : in Button_Callback.Marshallers.Void_Marshaller.Handler;
      From_Stock : in Boolean                                             := False)
     return Gtk.Button.Gtk_Button;

   function New_Button
     (Label      : in     String;
      Callback   : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      From_Stock : in     Boolean                                             := False)
     return Gtk.Button.Gtk_Button;

   function New_Column_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label;

   function New_Hseperator
     return Gtk.Separator.Gtk_Hseparator;

   function New_Label
     (Title : in String)
     return Gtk.Label.Gtk_Label;

   ---------------------------------------------------------------------------
   --  @deprecated
   --
   function New_Menu_Item
     (Label      : in String;
      Callback   : in Menu_Item_Callback.Marshallers.Void_Marshaller.Handler;
      From_Stock : in Boolean                                                := False)
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Menu_Item
     (Label      : in     String;
      Callback   : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      From_Stock : in     Boolean                                             := False)
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Menu_Separator
      return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Sub_Menu
     (Menu_Bar : access Gtk.Menu_Shell.Gtk_Menu_Shell_Record'Class;
      Label    : in String)
     return Gtk.Menu.Gtk_Menu;

   function New_Stock_Button
     (Stock_Id : in     String;
      Callback : in     Button_Callback.Marshallers.Void_Marshaller.Handler)
     return Gtk.Button.Gtk_Button;

   function New_Stock_Button
     (Stock_Id : in     String;
      Callback : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Button.Gtk_Button;

   function New_Stock_Menu_Item
     (Stock_Id   : in String;
      Callback   : in Menu_Item_Callback.Marshallers.Void_Marshaller.Handler)
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Stock_Menu_Item
     (Stock_Id : in     String;
      Callback : in     Widget_Callback.Marshallers.Void_Marshaller.Handler;
      Widget   : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_TearOff_Menu_Item
     return Gtk.Menu_Item.Gtk_Menu_Item;

   function New_Vpaned
     return Gtk.Paned.Gtk_Vpaned;

   procedure Set_Default
     (Window : access Gtk.Window.Gtk_Window_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------------------------------------------------------------
   --  Sets the icon of Window to the pixmap with Filename. Filename is
   --  searched in the shared icon directory. If icon can not be found, a
   --  warning is logged.
   procedure Set_Icon
     (Window   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filename : in     String);

   function To_Display_Name
     (Highlight_Status : in Projects.Subgraph_Highlight_Status)
     return String;

   function To_Display_Name
     (Highlight_Status : in Vis_Windows.Selection_Highlight_Status)
     return String;

end Giant.Gui_Utils;


