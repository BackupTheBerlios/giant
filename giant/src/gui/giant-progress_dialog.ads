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
--  $RCSfile: giant-progress_dialog.ads,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
-- Provides a progress dialog.
--
-- Emits the "cancelled" callback when the cancel button is pressed.
--

with Glib;
with Gtk.Adjustment;
with Gtk.Button;
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

   procedure Set_Cancel_Enabled
     (Dialog  : access Progress_Dialog_Record;
      Enabled : in     Boolean);

   procedure Set_Lower
     (Dialog : access Progress_Dialog_Record;
      Lower  : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets the message that is displayed above the progress bar.
   procedure Set_Message
     (Dialog  : access Progress_Dialog_Record;
      Message : in     String);

   procedure Set_Percentage
     (Dialog     : access Progress_Dialog_Record;
      Percentage : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets a format string used to display text indicating the
   --  current progress. The string can contain the following
   --  substitution characters:
   --
   --  %v - the current progress value.
   --  %l - the lower bound for the progress value.
   --  %u - the upper bound for the progress value.
   --  %p - the current progress percentage.
   --
   --  Text is only displayed in activity mode.
   procedure Set_Progress_Text
     (Dialog : access Progress_Dialog_Record;
      Text   : in     String);

   procedure Set_Upper (Dialog : access Progress_Dialog_Record;
                        Upper  : in     Glib.Gdouble);

   -------------------------------------------------------------------------
   --  Sets the current value. If value is higher than upper_bound,
   --  value mod upper_bound is set.
   procedure Set_Value (Dialog : access Progress_Dialog_Record;
                        Value  : in     Glib.Gdouble);

private
   type Progress_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Cancel_Button : Gtk.Button.Gtk_Button;
        Progress_Bar : Gtk.Progress_Bar.Gtk_Progress_Bar;
        Progress_Bar_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
        Progress_Label : Gtk.Label.Gtk_Label;
     end record;

end Giant.Progress_Dialog;
