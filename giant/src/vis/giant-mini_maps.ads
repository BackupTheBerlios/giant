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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-mini_maps.ads,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:57 $
--
------------------------------------------------------------------------------
--
--  This package contains the mini map used to display the current position
--  and size of the visual area within a graph widget.
--


with Gdk.Color;
with Gtk.Drawing_Area;
with Gdk.GC;
with Gdk.Pixmap;
with Gtk.Handlers;
with Giant.Graph_Widgets;
with Giant.Vis;

package Giant.Mini_Maps is

   type Mini_Map_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;

   type Mini_Map is access all Mini_Map_Record'Class;

   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null);

   procedure Initialize
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

   procedure Set_Graph_Widget
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget);

private

   procedure Update
     (Widget : access Mini_Map_Record);

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record);

   Default_Width  : constant := 100;
   Default_Height : constant :=  70;

   --  must start at index 0, Mini_Map_Colors'Images (E) must be
   --  recognized by Gdk.Color.Parse for every E : Mini_Map_Colors
   type Mini_Map_Colors is (Black, Red, White);

   type Mini_Map_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         Watched              : Graph_Widgets.Graph_Widget := null;
         Logical_Area_Handler : Gtk.Handlers.Handler_Id;
         Visible_Area_Handler : Gtk.Handlers.Handler_Id;
         Transformation       : Vis.Transformation_Type;
         Buffer               : Gdk.Pixmap.Gdk_Pixmap      :=
                                  Gdk.Pixmap.Null_Pixmap;
         Polluted             : Boolean                    := True;
         Background_Gc        : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Logical_Area_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Border_Gc    : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Visible_Fill_Gc      : Gdk.GC.Gdk_GC              := Gdk.GC.Null_GC;
         Colors               : Gdk.Color.Gdk_Color_Array
           (Mini_Map_Colors'Pos (Mini_Map_Colors'First) ..
            Mini_Map_Colors'Pos (Mini_Map_Colors'Last));
      end record;

end Giant.Mini_Maps;
