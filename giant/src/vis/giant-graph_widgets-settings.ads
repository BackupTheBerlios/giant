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
--  $RCSfile: giant-graph_widgets-settings.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/23 16:18:36 $
--
------------------------------------------------------------------------------


package Giant.Graph_Widgets.Settings is

   ------------------------------
   -- Initialization & Updates --
   ------------------------------

   ---------------------------------------------------------------------------
   --  Sets up all configuration-dependent settings:
   --  * highlight colors
   --  * node colors
   --  * edge colors
   --  * background colors
   --  Must be called one and only one time during the lifetime of a graph
   --  widget.
   procedure Set_Up
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access);

   ---------------------------------------------------------------------------
   --  Changes the style-dependent settings:
   --  * node colors
   --  * edge colors
   --  * background color
   procedure Set_Style
     (Widget : access Graph_Widget_Record'Class;
      Style  : in     Config.Vis_Styles.Visualisation_Style_Access);


   ------------
   -- Colors --
   ------------

   function Get_Highlight_Color
     (Widget       : access Graph_Widget_Record'Class;
      Highlighting : in     Vis_Data.Highlight_Type)
     return Gdk.Color.Gdk_Color;

   function Get_Background_Color
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Color.Gdk_Color;

   function Get_Edge_Style
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Edge_Style_Type;

   function Get_Edge_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color;

   function Show_Edge_Label
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Boolean;

   function Get_Edge_Label_Color
     (Widget       : access Graph_Widget_Record'Class;
      Edge         : in     Vis_Data.Vis_Edge_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Node_Border_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Node_Fill_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;

   function Get_Node_Text_Color
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Color.Gdk_Color;


   -----------
   -- Icons --
   -----------

   ---------------------------------------------------------------------------
   --  Returns True if the annotation icon should be shown, False otherwise.
   function Is_Annotated
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Gets the icon to be shown inside annotated nodes or 'Null_Pixmap'
   function Get_Annotation_Icon
     (Widget       : access Graph_Widget_Record'Class)
     return Gdk.Pixmap.Gdk_Pixmap;

   ---------------------------------------------------------------------------
   --  The icon to be shown for 'Node' or 'Null_Pixmap'
   function Get_Node_Icon
     (Widget       : access Graph_Widget_Record'Class;
      Node         : in     Vis_Data.Vis_Node_Id)
     return Gdk.Pixmap.Gdk_Pixmap;

end Giant.Graph_Widgets.Settings;