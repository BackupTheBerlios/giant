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
--  $RCSfile: giant-layout_dialog-widgets.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Gtk.Box;
with Gtk.Check_Button;
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
         Widget         : Gtk.Box.Gtk_Vbox;
         Root_Node      : Gtk.Gentry.Gtk_Entry;
         Class_Set_List : Gui_Utils.String_Clists.Giant_Data_Clist;
         Check          : Gtk.Check_Button.Gtk_Check_Button;
      end record;

   type Other_Layout_Container_Record is
     new Layout_Container_Record with
      record
         Widget : Gtk.Box.Gtk_Vbox;
         Layout_Name : Gtk.Gentry.Gtk_Entry;
         Parameters : Gtk.Gentry.Gtk_Entry;
      end record;

end Giant.Layout_Dialog.Widgets;
