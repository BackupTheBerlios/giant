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
--  $RCSfile: giant-basic_evolutions.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/09/11 18:44:24 $
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Gtk.Main;

with Giant.Logger;
with Giant.Progress_Dialog;
use type Giant.Progress_Dialog.Progress_Dialog_Access;

package body Giant.Basic_Evolutions is

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

   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
      return Boolean
     is
      Dead : Boolean;
   begin
      if (Individual = null) then
         return False;
      end if;

      Individual.Complexity := Individual.Complexity + Delta_Complexity;
      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Value (Individual.Dialog,
                                    Float (Individual.Complexity));
      end if;

      while Gtk.Main.Events_Pending loop
         Dead := Gtk.Main.Main_Iteration;
         pragma Assert (not Dead);
      end loop;

      return Individual.Cancelled;
   end Step;

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
   begin
      if (Individual = null) then
         return;
      end if;

      if (Individual.Dialog /= null) then
         Progress_Dialog.Set_Upper (Individual.Dialog, Float (Total_Complexity));
         Progress_Dialog.Set_Activity_Mode (Individual.Dialog, Total_Complexity /= 0);
      end if;
   end Set_Total_Complexity;

end Giant.Basic_Evolutions;
