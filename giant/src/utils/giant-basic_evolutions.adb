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
--  $RCSfile: giant-basic_evolutions.adb,v $, $Revision: 1.7 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Glib;
with Gtk.Main;

with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Progress_Dialog;
use type Giant.Progress_Dialog.Progress_Dialog_Access;

package body Giant.Basic_Evolutions is

   Update_Interval : constant := 1_000;

   package Logger is new Giant.Logger
     (Name => "Giant.Basic_Evolutions");

   package Basic_Evolution_Dialog_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Progress_Dialog.Progress_Dialog_Record,
      User_Type   => Basic_Evolution_Access);

   procedure Cancel_Callback
     (Dialog     : access Progress_Dialog.Progress_Dialog_Record'Class;
      Individual : in     Basic_Evolution_Access) is
   begin
      Individual.Cancelled := True;
   end Cancel_Callback;

   function Create
     (Dialog : in Progress_Dialog.Progress_Dialog_Access)
     return Basic_Evolution_Access
     is
      Individual : Basic_Evolution_Access;
   begin
      Individual := new Basic_Evolution;
      Individual.Dialog := Dialog;
      if (Dialog /= null) then
         Individual.Cancel_Handler :=
           Basic_Evolution_Dialog_Cb.Connect
           (Widget    => Dialog,
            Name      => "cancelled",
            Marsh     => Basic_Evolution_Dialog_Cb.To_Marshaller
            (Cancel_Callback'Access),
            User_Data => Individual);
         Progress_Dialog.Set_Activity_Mode (Dialog, True);
         Progress_Dialog.Show_All (Dialog);
      end if;
      return Individual;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Evolution, Basic_Evolution_Access);

   procedure Destroy
     (Individual : in out Basic_Evolution_Access)
   is
   begin
      if (Individual.Dialog /= null) then
         Progress_Dialog.Destroy (Individual.Dialog);
      end if;
      Free (Individual);
   end Destroy;

   function Iterate_Main
     (Individual   : in Basic_Evolution_Access;
      Force_Update : in Boolean)
     return Boolean
   is
     Dead : Boolean;
   begin
     if (Force_Update) then
        while Gtk.Main.Events_Pending loop
           Dead := Gtk.Main.Main_Iteration;
           exit when Dead;
        end loop;
     end if;

      return Individual.Cancelled;
   end Iterate_Main;

   function Should_Update
     (Individual   : in Basic_Evolution_Access)
     return Boolean
   is
     use type Ada.Real_Time.Time;
   begin
      if (Ada.Real_Time.Clock >= Individual.Last_Main_Iteration
          + Ada.Real_Time.Milliseconds (Update_Interval))
      then
         Individual.Last_Main_Iteration := Ada.Real_Time.Clock;
         return True;
      else
         return False;
      end if;
   end Should_Update;

   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
      return Boolean
   is
      Update : Boolean;
   begin
      if (Individual = null) then
         return False;
      end if;

      Individual.Complexity := Individual.Complexity + Delta_Complexity;
      Update := Should_Update (Individual);
      if (Individual.Dialog /= null and then Update) then
         Progress_Dialog.Set_Value (Individual.Dialog,
                                    Glib.Gdouble (Individual.Complexity));
      end if;

      return Iterate_Main (Individual, Update);
   end Step;

   procedure Set_Cancel_Enabled
     (Individual : in Basic_Evolution_Access;
      Enabled    : in Boolean)
   is
   begin
      if (Individual = null) then
         return;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Cancel_Enabled (Individual.Dialog, Enabled);
      end if;
   end Set_Cancel_Enabled;

   function Set_Percentage
     (Individual : in Basic_Evolution_Access;
      Percentage : in Float;
      Text       : in String := "")
     return Boolean
   is
   begin
      if (Individual = null) then
         return False;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Percentage (Individual.Dialog,
                                         Glib.Gdouble (Percentage));
         Progress_Dialog.Set_Activity_Mode (Individual.Dialog, False);
         Progress_Dialog.Set_Progress_Text (Individual.Dialog, Text);
      end if;

      return Iterate_Main (Individual, Force_Update => True);
   end Set_Percentage;

   procedure Set_Text
     (Individual : in Basic_Evolution_Access;
      Text       : in String)
   is
   begin
      if (Individual = null) then
         return;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Progress_Text (Individual.Dialog, Text);
      end if;
   end Set_Text;

   procedure Set_Total_Complexity
     (Individual       : in Basic_Evolution_Access;
      Total_Complexity : in     Natural)
   is
      Cancel : Boolean;
   begin
      if (Individual = null) then
         return;
      end if;

      Individual.Complexity := 0;
      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Upper (Individual.Dialog,
                                    Glib.Gdouble (Total_Complexity));
         Progress_Dialog.Set_Activity_Mode
           (Individual.Dialog, Total_Complexity = 0);
      end if;

      Cancel := Iterate_Main (Individual, Force_Update => True);
   end Set_Total_Complexity;

end Giant.Basic_Evolutions;
