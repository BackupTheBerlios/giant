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
--  $RCSfile: giant-layout_dialog.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Giant.Default_Dialog;
with Giant.Vis;

with Gtk.Notebook;
with Gtk.Widget;

package Giant.Layout_Dialog is

   ---------------------------------------------------------------------------
   --  Layout Container
   ---------------------------------------------------------------------------

   type Layout_Container_Record is abstract tagged private;

   type Layout_Container is access all Layout_Container_Record'Class;

   function Get_Widget
     (Container : access Layout_Container_Record)
     return Gtk.Widget.Gtk_Widget is abstract;

   function Get_Display_Name
     (Container : access Layout_Container_Record)
     return String is abstract;

   function Get_Layout_Name
     (Container : access Layout_Container_Record)
     return String is abstract;

   function Get_Layout_Parameters
     (Container : access Layout_Container_Record)
      return String is abstract;

   ---------------------------------------------------------------------------
   --  Layout Dialog
   ---------------------------------------------------------------------------

   type Layout_Dialog_Record (Container_Count : Integer) is
     new Default_Dialog.Default_Dialog_Record with private;

   type Layout_Dialog_Access is
      access all Layout_Dialog_Record'Class;

   procedure Create
     (Dialog         :    out Layout_Dialog_Access;
      Window_Name    : in     String;
      Selection_Name : in     String);

   procedure Initialize
     (Dialog         : access Layout_Dialog_Record'Class;
      Window_Name    : in     String;
      Selection_Name : in     String);

   procedure Apply_Layout
     (Dialog   : access Layout_Dialog_Record;
      Position : in     Vis.Logic.Vector_2d);

   function Can_Hide
     (Dialog : access Layout_Dialog_Record)
     return Boolean;

   procedure Show
     (Window_Name    : in String;
      Selection_Name : in String;
      Position       : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d);

private
   type Layout_Container_Record is abstract tagged null record;

   type Layout_Container_Array is array (Integer range <>)
     of Layout_Container;

   type Layout_Dialog_Record (Container_Count : Integer) is
     new Default_Dialog.Default_Dialog_Record
     with record
        Layouts_Notebook : Gtk.Notebook.Gtk_Notebook;
        Position : Vis.Logic.Vector_2d := Vis.Logic.Zero_2d;
        Selection_Name : Ada.Strings.Unbounded.Unbounded_String;
        Window_Name : Ada.Strings.Unbounded.Unbounded_String;
        Layouts : Layout_Container_Array (0 .. Container_Count);
        Layouts_Count : Natural := 0;
     end record;

end Giant.Layout_Dialog;
