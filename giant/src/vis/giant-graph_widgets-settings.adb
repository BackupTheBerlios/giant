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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets-settings.adb,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/23 16:18:36 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;

with Gdk.Bitmap;
with Gtkada.Types;

with Untagged_Ptr_Hash;
pragma Elaborate_All (Untagged_Ptr_Hash);

with Giant.Logger;

package body Giant.Graph_Widgets.Settings is

   Unknown_Color : exception;

   package Settings_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Settings");

   function Hash_Chars_Ptr_Array_Access
     (Key : in     Config.Chars_Ptr_Array_Access)
     return Integer is

      type Dummy_Type is null record;
      type Dummy_Access is access Dummy_Type;
      type Address_Record is record
         First  : Dummy_Access;
         Second : Dummy_Access;
      end record;
      pragma Pack (Address_Record);
      function To_Address_Record is new Ada.Unchecked_Conversion
        (Source => Config.Chars_Ptr_Array_Access,
         Target => Address_Record);
      function Hash_Dummy_Address is new Untagged_Ptr_Hash
        (T     => Dummy_Type,
         T_Ptr => Dummy_Access);

      Dummy_Record : Address_Record;
   begin
      Dummy_Record := To_Address_Record (Key);
      return Hash_Dummy_Address (Dummy_Record.Second);
   end Hash_Chars_Ptr_Array_Access;

   package Icon_Mappings is new Hashed_Mappings
     (Key_Type   => Config.Chars_Ptr_Array_Access,
      Hash       => Hash_Chars_Ptr_Array_Access,
      Value_Type => Gdk.Pixmap.Gdk_Pixmap);

   ---------------------------------------------------------------------------
   --  Manages the settings for highlight colors.
   --  These must be buffered because Giant.Config does not guarantee
   --  efficient polling.
   package Highlight_Colors is

      function Is_Initialized
        return Boolean;

      procedure Initialize;

      function Get_Color_Access
        (Highlighting : in     Vis_Data.Highlight_Type)
        return Config.Color_Access;

   private

      type Highlight_Colors_Array is array (Vis_Data.Highlight_Type) of
        Config.Color_Access;

      Highlight_Colors             : Highlight_Colors_Array;
      Highlight_Colors_Initialized : Boolean := False;

   end Highlight_Colors;

   ---------------------------------------------------------------------------
   --  Manages the Pixmaps for icons.
   --
   --  It is assumed that all graph widgets can share pixmaps to save
   --  resources. Actually this is can lead to problems because each
   --  pixmap should be inialized specially for one Gdk_Window.
   package Icons is

      function Is_Initialized
        return Boolean;

      procedure Initialize
        (Widget : access Graph_Widget_Record'Class);

      ------------------------------------------------------------------------
      --  Adds all icons needed for 'Style'
      procedure Add_Vis_Style
        (Widget : access Graph_Widget_Record'Class;
         Style : in     Config.Vis_Styles.Visualisation_Style_Access);

      function Get_Icon
        (Data   : in     Config.Chars_Ptr_Array_Access)
        return Gdk.Pixmap.Gdk_Pixmap;

      function Get_Annotation_Icon
        return Gdk.Pixmap.Gdk_Pixmap;

   private

      procedure Store_Icon
        (Widget : access Graph_Widget_Record'Class;
         Data   : in     Config.Chars_Ptr_Array_Access);

      Annotation_Access : Config.Chars_Ptr_Array_Access;
      Map               : Icon_Mappings.Mapping;
      Map_Initialized   : Boolean := False;
   end Icons;


   ---------------------------------------------------------------------------
   --  Creates a color if it does not exist yet.
   procedure Assure_Color
     (Widget       : access Graph_Widget_Record'Class;
      Color_Access : in     Config.Color_Access) is

      Color : Gdk.Color.Gdk_Color;
   begin
      if not Color_Mappings.Is_Bound
        (Widget.Settings.Color_Pool, Color_Access) then

         declare
            Color_Spec : String := Config.Get_Color_Value (Color_Access);
         begin
            Color := Gdk.Color.Parse (Color_Spec);
            Gdk.Color.Alloc
              (Colormap => Get_Colormap (Widget),
               Color    => Color);
         exception
            when Gdk.Color.Wrong_Color =>
               Settings_Logger.Error
                 ("Could not parse the color specification """ & Color_Spec &
                  """ or could not allocate that color. Will use black"
                  & " instead.");
               Color := Gdk.Color.Black (Get_Colormap (Widget));
         end;
         Color_Mappings.Bind (Widget.Settings.Color_Pool, Color_Access, Color);
      end if;
   end Assure_Color;

   ---------------------------------------------------------------------------
   --  Gets a color. If it is not created yet then creates it.
   procedure Find_Color
     (Widget       : access Graph_Widget_Record'Class;
      Color_Access : in     Config.Color_Access;
      Color        :    out Gdk.Color.Gdk_Color) is
   begin
      Color := Color_Mappings.Fetch
        (Widget.Settings.Color_Pool, Color_Access);
      --  The case "color" not yet allocated is handled in the exception
      --  part for performance reasons. Otherwise double lookup for
      --  every call would be necessary.
   exception
      when Color_Mappings.Not_Bound =>
         Assure_Color (Widget, Color_Access);
         Color := Color_Mappings.Fetch
           (Widget.Settings.Color_Pool, Color_Access);
   end Find_Color;

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      if not Highlight_Colors.Is_Initialized then
         Highlight_Colors.Initialize;
      end if;
      if not Icons.Is_Initialized then
         Icons.Initialize (Widget);
      end if;
      --  Create never changing colors
      for I in Vis_Data.Highlight_Type loop
         Assure_Color (Widget, Highlight_Colors.Get_Color_Access (I));
      end loop;
      --  Create style-specific data
      Set_Style (Widget, Style);
   end Set_Up;

   procedure Set_Style
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      Icons.Add_Vis_Style (Widget, Style);
   end Set_Style;

   function Get_Highlight_Color
     (Widget       : access Graph_Widget_Record'Class;
      Highlighting : in     Vis_Data.Highlight_Type)
     return Gdk.Color.Gdk_Color is

      Color : Gdk.Color.Gdk_Color;
   begin
      Find_Color
        (Widget,
         Highlight_Colors.Get_Color_Access (Highlighting),
         Color);
      return Color;
   end Get_Highlight_Color;

   function Get_Background_Color
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Color.Gdk_Color is

      Color : Gdk.Color.Gdk_Color;
   begin
      Find_Color
        (Widget,
         Config.Vis_Styles.Get_Vis_Window_Background_Color
           (Get_Vis_Style (Widget)),
         Color);
      return Color;
   end Get_Background_Color;

   function Get_Edge_Style
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Edge_Style_Type is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      return Edge_Style_Type (Config.Vis_Styles.Get_Line_Style
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge)));
   end Get_Edge_Style;

   function Get_Edge_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color is

      Graph_Edge   : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
      Color_Access : Config.Color_Access;
      Color        : Gdk.Color.Gdk_Color;
   begin
      Color_Access := Config.Vis_Styles.Get_Line_Color
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge));
      Find_Color (Widget, Color_Access, Color);
      return Color;
   end Get_Edge_Color;

   function Show_Edge_Label
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Boolean is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      return Config.Vis_Styles.Show_Label_For_Edge_Class_Name
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge));
   end Show_Edge_Label;

   function Get_Edge_Label_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color is

      Graph_Edge   : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
      Color_Access : Config.Color_Access;
      Color        : Gdk.Color.Gdk_Color;
   begin
      Color_Access := Config.Vis_Styles.Get_Text_Color
        (Vis_Style  => Get_Vis_Style (Widget),
         Edge_Class => Graph_Lib.Get_Edge_Class_Id (Graph_Edge));
      Find_Color (Widget, Color_Access, Color);
      return Color;
   end Get_Edge_Label_Color;

   function Get_Node_Border_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      Graph_Node   : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
      Color_Access : Config.Color_Access;
      Color        : Gdk.Color.Gdk_Color;
   begin
      Color_Access := Config.Vis_Styles.Get_Border_Color
        (Vis_Style  => Get_Vis_Style (Widget),
         Node_Class => Graph_Lib.Get_Node_Class_Id (Graph_Node));
      Find_Color (Widget, Color_Access, Color);
      return Color;
   end Get_Node_Border_Color;

   function Get_Node_Fill_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      Graph_Node   : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
      Color_Access : Config.Color_Access;
      Color        : Gdk.Color.Gdk_Color;
   begin
      Color_Access := Config.Vis_Styles.Get_Fill_Color
        (Vis_Style  => Get_Vis_Style (Widget),
         Node_Class => Graph_Lib.Get_Node_Class_Id (Graph_Node));
      Find_Color (Widget, Color_Access, Color);
      return Color;
   end Get_Node_Fill_Color;

   function Get_Node_Text_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color is

      Graph_Node   : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
      Color_Access : Config.Color_Access;
      Color        : Gdk.Color.Gdk_Color;
   begin
      Color_Access := Config.Vis_Styles.Get_Text_Color
        (Vis_Style  => Get_Vis_Style (Widget),
         Node_Class => Graph_Lib.Get_Node_Class_Id (Graph_Node));
      Find_Color (Widget, Color_Access, Color);
      return Color;
   end Get_Node_Text_Color;


   function Is_Annotated
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean is
   begin
      return False;
   end Is_Annotated;

   function Get_Annotation_Icon
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Pixmap.Gdk_Pixmap is
   begin
      return Icons.Get_Annotation_Icon;
   end Get_Annotation_Icon;

   function Get_Node_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Pixmap.Gdk_Pixmap is

      Node_Class : Graph_Lib.Node_Class_Id;
      Data       : Config.Chars_Ptr_Array_Access;
   begin
      Node_Class := Graph_Lib.Get_Node_Class_Id
        (Node       => Vis_Data.Get_Graph_Node (Node));
      Data := Config.Vis_Styles.Get_Node_Icon
        (Vis_Style  => Get_Vis_Style (Widget),
         Node_Class => Node_Class);
      return Icons.Get_Icon (Data);
   end Get_Node_Icon;


   ----------------------
   -- Highlight Colors --
   ----------------------

   package body Highlight_Colors is

      function Is_Initialized
        return Boolean is
      begin
         return Highlight_Colors_Initialized;
      end Is_Initialized;

      procedure Initialize is
      begin
         Highlight_Colors :=
           (Vis_Data.Current_Local =>
              Config.Return_Highlight_Color_For_Actual_Selection,
            Vis_Data.First_Local   =>
              Config.Return_Highlight_Color_For_Selection (Config.Color_1),
            Vis_Data.Second_Local  =>
              Config.Return_Highlight_Color_For_Selection (Config.Color_2),
            Vis_Data.Third_Local   =>
              Config.Return_Highlight_Color_For_Selection (Config.Color_3),
            Vis_Data.First_Global  =>
              Config.Return_Highlight_Color_For_IML_Subgraph (Config.Color_1),
            Vis_Data.Second_Global =>
              Config.Return_Highlight_Color_For_IML_Subgraph (Config.Color_2),
            Vis_Data.Third_Global  =>
              Config.Return_Highlight_Color_For_IML_Subgraph (Config.Color_3));
         Highlight_Colors_Initialized := True;
      end Initialize;

      function Get_Color_Access
        (Highlighting : in     Vis_Data.Highlight_Type)
        return Config.Color_Access is
      begin
         return Highlight_Colors (Highlighting);
      end Get_Color_Access;

   end Highlight_Colors;


   -----------
   -- Icons --
   -----------

   package body Icons is

      function Is_Initialized
        return Boolean is
      begin
         return Map_Initialized;
      end Is_Initialized;

      procedure Initialize
        (Widget : access Graph_Widget_Record'Class) is
      begin
         Map := Icon_Mappings.Create;
         Annotation_Access := Config.Return_Icon_For_Node_Annotations;
         Store_Icon (Widget, Annotation_Access);
         Map_Initialized := True;
      end Initialize;

      procedure Add_Vis_Style
        (Widget : access Graph_Widget_Record'Class;
         Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is

         package Id_Sets renames Graph_Lib.Node_Class_Id_Sets;
         All_Ids      : Id_Sets.Set := Graph_Lib.Get_All_Node_Class_Ids;
         Iterator     : Id_Sets.Iterator;
         Current_Icon : Config.Chars_Ptr_Array_Access;
      begin
         Iterator := Id_Sets.Make_Iterator (All_Ids);
         while Id_Sets.More (Iterator) loop
            Current_Icon := Config.Vis_Styles.Get_Node_Icon
              (Vis_Style  => Style,
               Node_Class => Id_Sets.Current (Iterator));
            Store_Icon (Widget, Current_Icon);
            Id_Sets.Next (Iterator);
         end loop;
         Id_Sets.Destroy (Iterator);
         Id_Sets.Destroy (All_Ids);
      end Add_Vis_Style;

      function Get_Icon
        (Data : in     Config.Chars_Ptr_Array_Access)
        return Gdk.Pixmap.Gdk_Pixmap is
      begin
         return Icon_Mappings.Fetch (Map, Data);
      exception
         when Icon_Mappings.Not_Bound =>
            return Gdk.Pixmap.Null_Pixmap;
      end Get_Icon;

      function Get_Annotation_Icon
        return Gdk.Pixmap.Gdk_Pixmap is
      begin
         return Get_Icon (Annotation_Access);
      end Get_Annotation_Icon;

      procedure Store_Icon
        (Widget : access Graph_Widget_Record'Class;
         Data   : in     Config.Chars_Ptr_Array_Access) is

         Pixmap : Gdk.Pixmap.Gdk_Pixmap;
         Mask   : Gdk.Bitmap.Gdk_Bitmap := Gdk.Bitmap.Null_Bitmap;
      begin
         if Config."/=" (Data, null) and then
           not Icon_Mappings.Is_Bound (Map, Data) then

            Gdk.Pixmap.Create_From_Xpm_D
              (Pixmap      => Pixmap,
               Window      => Get_Window (Widget),
               Mask        => Mask,
               Transparent => Gdk.Color.Null_Color,
               Data        => Data.all);

            Icon_Mappings.Bind (Map, Data, Pixmap);
         end if;
      end Store_Icon;

   end Icons;


end Giant.Graph_Widgets.Settings;
