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
--  $RCSfile: giant-graph_widgets-settings.adb,v $, $Revision: 1.5 $
--  $Author: keulsn $
--  $Date: 2003/06/27 16:58:58 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;

with Gdk.Bitmap;
with Gtkada.Types;

with Untagged_Ptr_Hash;
pragma Elaborate_All (Untagged_Ptr_Hash);

with Giant.Controller;
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
   --
   package Colors is

      procedure Set_Up_Color_Array
        (Widget : access Graph_Widget_Record'Class);

      function Get_Color
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer);

      function Get_Highlight_Color
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type);

      procedure Shut_Down_Color_Array
        (Widget : access Graph_Widget_Record'Class);

   end Colors;

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
         Style  : in     Config.Vis_Styles.Visualisation_Style_Access);

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

   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access) is
   begin
      if not Icons.Is_Initialized then
         Icons.Initialize (Widget);
      end if;
      Colors.Set_Up_Color_Array (Widget);
      --  Create style-specific data
      Icons.Add_Vis_Style (Widget, Style);
   end Set_Up;

   function Get_Highlight_Color
     (Widget       : access Graph_Widget_Record'Class;
      Highlighting : in     Vis_Data.Highlight_Type)
     return Gdk.Color.Gdk_Color is

      Color : Gdk.Color.Gdk_Color;
   begin
      return Colors.Get_Highlight_Color (Widget, Highlighting;
   end Get_Highlight_Color;

   function Get_Background_Color
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Color.Gdk_Color is
   begin
      return Colors.Get_Color
        (Config.Vis_Styles.Get_Vis_Window_Background_Color
         (Widget.Settings.Vis_Style));
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
      return Controller.Is_Node_Annotated (Vis_Data.Get_Graph_Node (Node));
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


   ------------
   -- Colors --
   ------------

   package body Colors is

      function Create
        (Color_Access : in     Config.Color_Access)
        return Gdk.Color.Gdk_Color is

         Color_Spec : String := Config.Get_Color_Value (Color_Access);
      begin
         return Gdk.Color.Parse (Color_Spec);
      exception
         when Gdk.Color.Wrong_Color =>
            Settings_Logger.Error
              ("Color """ & Color_Spec & """ could not be parsed. Use default"
               & " color instead.");
            return Gdk.Color.Null_Color;
      end Create;

      function Get_Highlight_Index
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type)
        return Natural is
      begin
         return Widget.Settings.Highlight_Index_Offset + Vis_Data.Pos (Light);
      end Get_Highlight_Index;

      procedure Set_Up_Highlight_Colors
        (Widget : access Graph_Widget_Record'Class) is
      begin
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.Current_Local)) :=
           Create (Config.Get_Current_Selection_Highlight_Color);
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.First_Local)) :=
           Create (Config.Get_Selection_Highlight_Color (Config.Color_1));
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.Second_Local)) :=
           Create (Config.Get_Selection_Highlight_Color (Config.Color_2));
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.Third_Local)) :=
           Create (Config.Get_Selection_Highlight_Color (Config.Color_3));
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.First_Global)) :=
           Create (Config.Get_Subgraph_Highlight_Color (Config.Color_1));
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.Second_Global)) :=
           Create (Config.Get_Subgraph_Highlight_Color (Config.Color_2));
         Widget.Settings.All_Colors
           (Highlight_Index (Vis_Data.Third_Global)) :=
           Create (Config.Get_Subgraph_Highlight_Color (Config.Color_3));
      end Set_Up_Highlight_Colors;

      procedure Set_Up_Color_Array
        (Widget : access Graph_Widget_Record'Class) is

         Color_Accesses : Config.Vis_Styles.Color_Access_Array_Access :=
           Config.Vis_Styles.Get_All_Colors;
      begin
         if Widget.Settings.All_Colors /= null then
            Shut_Down_Color_Array (Widget);
         end if;

         Widget.Settings.Highlight_Index_Offset := Style_Color_Accesses'Last -
           Vis_Data.Highlight_Type'Pos (Vis_Data.Highlight_Type'First) + 1;
         Widget.Settings.All_Colors := new Gdk.Color.Gdk_Color_Array
           (Style_Color_Accesses'First .. Style_Color_Accesses'Last +
                                            Vis_Data.Highlight_Type'Length);
         Set_Up_Highlight_Colors (Widget);
         for I in Style_Color_Accesses'Range loop
            Widget.Settings.All_Colors := Create_Color
              (Widget,
               Config.Get_Color_Value (Style_Color_Accesses (I)));
         end loop;

         declare
            Success      : Gdk.Color.Boolean_Array
              (Widget.Settings.All_Colors'Range);
            Result_Count : Glib.Gint;
         begin
            Gdk.Color.Alloc_Colors
              (Colormap => Get_Colormap (Widget),
               Colors   => Widget.Settings.All_Colors.all,
               Success  => Success,
               Result   => Result_Count);
            if Result_Count < Widget.Settings.All_Color'Length then
               Settings_Logger.Error
                 (Glib.Gint'Image (Success'Length - Result_Count) & " colors "
                  & "could not be allocated. Using default colors instead.");
               for I in Success'Range loop
                  if not Success (I) then
                     Widget.Settings.All_Colors (I) := Gdk.Color.Null_Color;
                  end if;
               end loop;
            end if;
         end;
      end Set_Up_Color_Array;

      function Get_Color
        (Widget : access Graph_Widget_Record'Class;
         Index  : in     Integer)
        return Gdk.Color.Gdk_Color is
      begin
         pragma Assert
           (Index < Widget.Settings.Hightlight_Index_Offset +
              Vis_Data.Highlight_Type'Pos
              (Vis_Data.Highlight_Type'First));
         return Widget.Settings.All_Colors (Index);
      end Get_Color;

      function Get_Highlight_Color
        (Widget : access Graph_Widget_Record'Class;
         Light  : in     Vis_Data.Highlight_Type)
        return Gdk.Color.Gdk_Color is
      begin
         return Widget.Settings.All_Colors
           (Vis_Data.Highlight_Type'Pos (Light) +
            Widget.Settings.Highlight_Index_Offset);
      end Get_Highlight_Color;

      procedure Shut_Down_Color_Array
        (Widget : access Graph_Widget_Record'Class) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Gdk.Color.Gdk_Color_Array,
            Name   => Color_Array_Access);

      begin
         for I in Widget.Settings.All_Colors'Range loop
            Gdk.Color.Free_Colors
              (Get_Colormap (Widget), Widget.Settings.All_Colors.all);
            Free (Widget.Settings.All_Colors);
         end loop;
      end Shut_Down_Color_Array;

   end Colors;


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
