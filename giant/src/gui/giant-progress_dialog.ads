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
--  $RCSfile: giant-progress_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/09/09 15:31:24 $
--
------------------------------------------------------------------------------
--
-- Provides a progress dialog.
--
-- Emits the "cancelled" callback when the cancel button is pressed.
--

with Gtk.Adjustment;
with Gtk.Label;
with Gtk.Progress_Bar;
with Gtk.Window;

with Giant.Default_Dialog;

package Giant.Progress_Dialog is

   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Progress_Dialog_Access is access all Progress_Dialog_Record'Class;

   procedure Create
     (Dialog  :    out Progress_Dialog_Access;
      Title   : in     String;
      Message : in     String);

   procedure Initialize
     (Dialog  : access Progress_Dialog_Record'Class;
      Title   : in     String;
      Message : in     String);

   function Can_Hide
     (Dialog : access Progress_Dialog_Record)
     return Boolean;

   function Get_Activity_Mode
     (Dialog : access Progress_Dialog_Record)
     return Boolean;

   procedure Set_Activity_Mode
     (Dialog        : access Progress_Dialog_Record;
      Activity_Mode : in     Boolean);

   procedure Set_Lower (Dialog : access Progress_Dialog_Record;
                        Lower  : in     Float);

   procedure Set_Percentage
     (Dialog     : access Progress_Dialog_Record;
      Percentage : in     Float);

   -------------------------------------------------------------------------
   --  Sets a format string used to display text indicating the
   --  current progress. The string can contain the following
   --  substitution characters:
   --
   --  %v - the current progress value.
   --  %l - the lower bound for the progress value.
   --  %u - the upper bound for the progress value.
   --  %p - the current progress percentage.
   procedure Set_Progress_Text
     (Dialog : access Progress_Dialog_Record;
      Text   : in     String);

   procedure Set_Upper (Dialog : access Progress_Dialog_Record;
                        Upper  : in     Float);


   -------------------------------------------------------------------------
   --  Sets the current value. If value is higher than upper_bound,
   --  value mod upper_bound is set.
   procedure Set_Value (Dialog : access Progress_Dialog_Record;
                        Value  : in     Float);

private
   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
        Progress_Bar_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
        Progress_Label : Gtk.Label.Gtk_Label;
     end record;

end Giant.Progress_Dialog;
