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
--  $RCSfile: giant-layout_dialog-widgets.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/07/08 16:07:32 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Gtk.Box;
with Gtk.Gentry;

with Giant.Gui_Utils;

package Giant.Layout_Dialog.Widgets is

   ---------------------------------------------------------------------------
   --  Matrix Layout
   ---------------------------------------------------------------------------

   type Matrix_Layout_Container_Record is
     new Layout_Container_Record with private;

   type Matrix_Layout_Container_Access is
     access all Matrix_Layout_Container_Record'Class;

   function Create_Matrix
     return Matrix_Layout_Container_Access;

   function Get_Widget
     (Container : access Matrix_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget;

   function Get_Display_Name
     (Container : access Matrix_Layout_Container_Record)
     return String;

   function Get_Layout_Name
     (Container : access Matrix_Layout_Container_Record)
     return String;

   function Get_Layout_Parameters
     (Container : access Matrix_Layout_Container_Record)
      return String;

   ---------------------------------------------------------------------------
   --  Tree Layout
   ---------------------------------------------------------------------------

   type Tree_Layout_Container_Record is
     new Layout_Container_Record with private;

   type Tree_Layout_Container_Access is
     access all Tree_Layout_Container_Record'Class;

   function Create_Tree
     return Tree_Layout_Container_Access;

   function Get_Widget
     (Container : access Tree_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget;

   function Get_Display_Name
     (Container : access Tree_Layout_Container_Record)
     return String;

   function Get_Layout_Name
     (Container : access Tree_Layout_Container_Record)
     return String;

   function Get_Layout_Parameters
     (Container : access Tree_Layout_Container_Record)
      return String;

   ---------------------------------------------------------------------------
   --  Other Layout
   ---------------------------------------------------------------------------

   type Other_Layout_Container_Record is
     new Layout_Container_Record with private;

   type Other_Layout_Container_Access is
     access all Other_Layout_Container_Record'Class;

   function Create_Other
     return Other_Layout_Container_Access;

   function Get_Widget
     (Container : access Other_Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget;

   function Get_Display_Name
     (Container : access Other_Layout_Container_Record)
     return String;

   function Get_Layout_Name
     (Container : access Other_Layout_Container_Record)
     return String;

   function Get_Layout_Parameters
     (Container : access Other_Layout_Container_Record)
      return String;

private

   type Matrix_Layout_Container_Record is
     new Layout_Container_Record with
      record
         Widget : Gtk.Box.Gtk_Vbox;
      end record;

   type Tree_Layout_Container_Record is
     new Layout_Container_Record with
      record
         Widget : Gtk.Box.Gtk_Vbox;
         Class_Set_List : Gui_Utils.String_Clists.Giant_Data_Clist;
      end record;

   type Other_Layout_Container_Record is
     new Layout_Container_Record with
      record
         Widget : Gtk.Box.Gtk_Vbox;
         Layout_Name : Gtk.Gentry.Gtk_Entry;
         Parameters : Gtk.Gentry.Gtk_Entry;
      end record;

end Giant.Layout_Dialog.Widgets;
