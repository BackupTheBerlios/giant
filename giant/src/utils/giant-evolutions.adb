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
--  $RCSfile: giant-evolutions.adb,v $, $Revision: 1.2 $
--  $Author: keulsn $
--  $Date: 2003/06/01 22:06:52 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;
with Ada.Tags;

with Gdk.Main;
with Gdk.Threads;
with Glib;
with Gtk.Main;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Ptr_Hashs;
pragma Elaborate_All (Ptr_Hashs);
with Ptr_Ops;

with Giant.Fixed_Priority_Queues;
with Giant.Logger;

package body Giant.Evolutions is


   package Evolution_Logger is new Giant.Logger
     (Name => "Giant.Evolutions");


   -------------------------------
   -- Driver_Controller         --
   -- for concurrent evolutions --
   -------------------------------

   --  Mapping Button --> Driver
   package Button_Hashs is new Ptr_Hashs
     (T     => Gtk.Button.Gtk_Button_Record,
      T_Ptr => Gtk.Button.Gtk_Button);
   package Button_Mappings is new Hashed_Mappings
     (Key_Type   => Gtk.Button.Gtk_Button,
      Hash       => Button_Hashs.Integer_Hash,
      Value_Type => Driver_Id_Type);

   --  Cancel_Button handling
   package Button_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk.Button.Gtk_Button_Record);
   procedure Cancel_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class);


   --  Reflect priority ordering on driver states to ids of driver states
   function Has_Higher_Priority_Id
     (Left  : in Driver_Id_Type;
      Right : in Driver_Id_Type)
     return Boolean;

   procedure Set_Position
     (Driver_Id : in     Driver_Id_Type;
      Position  : in     Natural);

   function Get_Position
     (Driver_Id : in Driver_Id_Type)
     return Natural;

   --  Priority queues to handle updates of drivers
   package Driver_State_Queues is new Giant.Fixed_Priority_Queues
     (Max_Size            => Number_Of_Slots,
      Item_Type           => Driver_Id_Type,
      Has_Higher_Priority => Has_Higher_Priority_Id,
      Set_Position        => Set_Position,
      Get_Position        => Get_Position);

   --  Stores all manageable Drivers
   type Driver_Array is array (Driver_Id_Type) of Driver_State_Type;


   --  Protected unit used for Driver control.
   protected Driver_Controller is

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Start_Driver
        (Individual      : access Concurrent_Evolution'Class;
         Started         :    out Boolean;
         Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog;
         Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar;
         Progress_Text   : in     Gtk.Label.Gtk_Label;
         Progress_Cancel : in     Gtk.Button.Gtk_Button);

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Perform_Update
        (Must_Synchronize :    out Concurrent_Driver_Access);

      entry Set_Individual
        (Driver_Id       : in     Driver_Id_Type;
         Individual      : access Concurrent_Evolution'Class);

      entry Schedule_Synchronization
        (Driver_Id       : in     Driver_Id_Type);

      entry Driver_Died
        (Driver_Id       : in     Driver_Id_Type);

      --  Schedules the cancel request for the driver associated with
      --  the given button. This operation cannot be split in an
      --  "Get_Driver_Id" and "Schedule_Cancel (Driver_Id)" because
      --  Driver_Id might become invalid before the call to Schedule_Cancel.
      procedure Schedule_Cancel
        (Progress_Cancel : in     Gtk.Button.Gtk_Button);

      function Is_Canceled
        (Driver_Id       : in     Driver_Id_Type)
        return Boolean;

   private

      --  All managed drivers is implemented as a global variable to simplify
      --  usage of Update_Queue.
      --  Drivers : Driver_Array

      --  Updates are done in order of 'Update_Queue'. All managed Drivers are
      --  in that queue at any point of time.
      Update_Queue : Driver_State_Queues.Queue_Type;

      --  Mapping Cancel_Button --> Driver_Id
      Cancel_Mapping : Button_Mappings.Mapping := Button_Mappings.Create;

   end Driver_Controller;


   ------------------------------
   -- Controlling              --
   -- for iterative evolutions --
   ------------------------------

   procedure Start_Iterative_Driver
     (Individual      : access Iterative_Evolution'Class;
      Started         :    out Boolean;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar;
      Progress_Text   : in     Gtk.Label.Gtk_Label;
      Progress_Cancel : in     Gtk.Button.Gtk_Button);

   procedure Stop_Iterative_Driver;


   ---------------
   -- Evolution --
   ---------------

   procedure Initialize
     (Individual : access Evolution;
      Complexity : in     Natural := 0) is
   begin
      Individual.Progress_Count := 0;
      Individual.Complexity := Complexity;
      Individual.Child_Progress := 0;
      Individual.Next_Action := Run;
      Individual.Parent := null;
   end Initialize;

   procedure Synchronized_Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action) is
   begin
      Next_Action := Run;
   end Synchronized_Step;

   procedure Set_Complexity
     (Individual   : access Evolution'Class;
      Total_Number : in     Natural) is
   begin
      Individual.Complexity := Total_Number;
   end Set_Complexity;

   procedure Advance_Progress
     (Individual : access Evolution'Class;
      Progress   : in     Natural) is
   begin
      Individual.Progress_Count := Individual.Progress_Count + Progress;
   end Advance_Progress;

   function Get_Progress_Count
     (Individual   : access Evolution'Class)
      return Natural is
   begin
      return Individual.Progress_Count;
   end Get_Progress_Count;

   function Get_Complexity
     (Individual   : access Evolution'Class)
      return Natural is
   begin
      return Individual.Complexity;
   end Get_Complexity;

   procedure Update_Visuals
     (Individual    : access Evolution'Class;
      Progress_Bar  : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text : in     Gtk.Label.Gtk_Label               := null) is

      Current          : Evolution_Class_Access :=
        Evolution_Class_Access (Individual);
      Complexity_Known : Boolean := True;
      Complexity       : Natural := 0;
      Step_Count       : Natural := 0;
      Progress         : Glib.Gfloat;
   begin
      while Current /= null loop
         if Current.Complexity = 0 then
            Complexity_Known := False;
         end if;
         Complexity := Complexity + Current.Complexity
           + Current.Child_Progress;
         Step_Count := Step_Count + Current.Progress_Count
           + Current.Child_Progress;
         Current := Get_Parent (Current);
      end loop;

      if Complexity_Known then
         if Gtk.Progress_Bar."/=" (Progress_Bar, null) then
            Progress := Glib."/"
              (Glib.Gfloat (Step_Count), Glib.Gfloat (Complexity));
            Gtk.Progress_Bar.Update
              (Progress_Bar => Progress_Bar,
               Percentage   => Progress);
         end if;
         if Gtk.Label."/=" (Progress_Text, null) then
            Gtk.Label.Set_Text
              (Label => Progress_Text,
               Str   => Natural'Image (Step_Count) & (-" of")
                        & Natural'Image (Complexity));
         end if;
      else
         if Gtk.Label."/=" (Progress_Text, null) then
            Gtk.Label.Set_Text
              (Label => Progress_Text,
               Str   => Natural'Image (Step_Count));
         end if;
      end if;
   end Update_Visuals;

   procedure Add_Child_Progress
     (Individual    : access Evolution'Class;
      Progress      : in     Natural) is
   begin
      Individual.Child_Progress := Individual.Child_Progress + Progress;
   end Add_Child_Progress;

   procedure Done
     (Individual    : access Evolution'Class;
      Canceled      : in     Boolean) is

      Parent : Evolution_Class_Access := Get_Parent (Individual);
   begin
      if Parent /= null then
         Add_Child_Progress (Parent, Get_Progress_Count (Individual));
         Set_Child (Parent, null);
      end if;
      Finish (Individual, Canceled);
   end Done;

   procedure Set_Next_Action
     (Individual  : access Evolution'Class;
      Next_Action : in     Evolution_Action) is

      package Evolution_Ptr_Ops is new Ptr_Ops
        (T     => Evolution,
         T_Ptr => Evolution_Class_Access);

   begin
      --  If bound checks are disabled then Next_Action may be out of
      --  range.
      if Next_Action in Evolution_Action then
         Individual.Next_Action := Next_Action;
      else
         Evolution_Logger.Error
           ("Individual"
            & Evolution_Ptr_Ops.Image (Evolution_Class_Access (Individual))
            & " of type " & Ada.Tags.External_Tag (Individual'Tag)
            & " requested an illegal Next_Action (Pos ="
            & Integer'Image (Evolution_Action'Pos (Next_Action))
            & "). Use Cancel instead.");
         Individual.Next_Action := Cancel;
      end if;
   end Set_Next_Action;

   function Get_Next_Action
     (Individual  : access Evolution'Class)
     return Evolution_Action is
   begin
      return Individual.Next_Action;
   end Get_Next_Action;


   procedure Set_Parent
     (Individual : access Evolution'Class;
      Parent     : Evolution_Class_Access) is
   begin
      Individual.Parent := Parent;
   end Set_Parent;

   function Get_Parent
     (Individual : access Evolution'Class)
     return Evolution_Class_Access is
   begin
      return Individual.Parent;
   end Get_Parent;

   function Has_Parent
     (Individual : access Evolution'Class)
     return Boolean is
   begin
      return Individual.Parent /= null;
   end Has_Parent;


   procedure Set_Child
     (Individual : access Evolution'Class;
      Child      : in     Evolution_Class_Access) is
   begin
      Individual.Child := Child;
   end Set_Child;

   function Get_Child
     (Individual : access Evolution'Class)
     return Evolution_Class_Access is
   begin
      return Individual.Child;
   end Get_Child;

   function Has_Child
     (Individual : access Evolution'Class)
     return Boolean is
   begin
      return Individual.Child /= null;
   end Has_Child;


   --------------------------
   -- Concurrent_Evolution --
   --------------------------

   procedure Start_Calculation
     (Individual      : access Concurrent_Evolution;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog             := null;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : in     Gtk.Label.Gtk_Label               := null;
      Progress_Cancel : in     Gtk.Button.Gtk_Button             := null;
      Started         :    out Boolean) is
   begin
      Driver_Controller.Start_Driver
        (Individual,
         Started,
         Progress_Dialog,
         Progress_Bar,
         Progress_Text,
         Progress_Cancel);
   end Start_Calculation;

   procedure Start_Sub_Calculation
     (Master          : access Concurrent_Evolution'Class;
      Sub_Calculation : access Concurrent_Evolution'Class) is

      Sub_Insert_Point : Concurrent_Evolution_Class_Access :=
        Concurrent_Evolution_Class_Access (Sub_Calculation);
      Master_Child     : Concurrent_Evolution_Class_Access :=
        Get_Concurrent_Child (Master);
   begin
      while Has_Child (Sub_Insert_Point) loop
         Sub_Insert_Point := Get_Concurrent_Child (Sub_Insert_Point);
      end loop;
      Set_Concurrent_Child (Sub_Insert_Point, Master_Child);
      if Master_Child /= null then
         Set_Concurrent_Parent (Master_Child, Sub_Insert_Point);
      end if;
      Set_Concurrent_Child
        (Master,
         Concurrent_Evolution_Class_Access (Sub_Calculation));
      Set_Concurrent_Parent
        (Sub_Calculation,
         Concurrent_Evolution_Class_Access (Master));
   end Start_Sub_Calculation;

   procedure Set_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class;
      Child      : in     Concurrent_Evolution_Class_Access) is
   begin
      Set_Child (Individual, Evolution_Class_Access (Child));
   end Set_Concurrent_Child;

   function Get_Concurrent_Child
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access is

      Child : Evolution_Class_Access := Get_Child (Individual);
   begin
      pragma Assert
        (Child = null or else
         Child.all in Concurrent_Evolution'Class);
      return Concurrent_Evolution_Class_Access (Child);
   end Get_Concurrent_Child;


   procedure Set_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class;
      Parent     : in     Concurrent_Evolution_Class_Access) is
   begin
      Set_Parent (Individual, Evolution_Class_Access (Parent));
   end Set_Concurrent_Parent;

   function Get_Concurrent_Parent
     (Individual : access Concurrent_Evolution'Class)
     return Concurrent_Evolution_Class_Access is

      Parent : Evolution_Class_Access := Get_Parent (Individual);
   begin
      pragma Assert
        (Parent = null or else
         Parent.all in Concurrent_Evolution'Class);
      return Concurrent_Evolution_Class_Access (Parent);
   end Get_Concurrent_Parent;


   -------------------------
   -- Iterative_Evolution --
   -------------------------

   procedure Start_Calculation
     (Individual      : access Iterative_Evolution;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog             := null;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : in     Gtk.Label.Gtk_Label               := null;
      Progress_Cancel : in     Gtk.Button.Gtk_Button             := null;
      Started         :    out Boolean) is
   begin
      Start_Iterative_Driver
        (Individual,
         Started,
         Progress_Dialog,
         Progress_Bar,
         Progress_Text,
         Progress_Cancel);
   end Start_Calculation;

   procedure Start_Sub_Calculation
     (Master          : access Iterative_Evolution'Class;
      Sub_Calculation : access Evolution'Class) is

      Sub_Insert_Point : Evolution_Class_Access :=
        Evolution_Class_Access (Sub_Calculation);
      Master_Child     : Evolution_Class_Access :=
        Get_Child (Master);
   begin
      while Has_Child (Sub_Insert_Point) loop
         Sub_Insert_Point := Get_Child (Sub_Insert_Point);
      end loop;
      Set_Child (Sub_Insert_Point, Master_Child);
      if Master_Child /= null then
         Set_Parent (Master_Child, Sub_Insert_Point);
      end if;
      Set_Child (Master, Evolution_Class_Access (Sub_Calculation));
      Set_Parent (Sub_Calculation, Evolution_Class_Access (Master));
   end Start_Sub_Calculation;


   -----------------------
   -- Concurrent_Driver --
   -----------------------

   task body Concurrent_Driver is
      Individual     : Concurrent_Evolution_Class_Access :=
        Concurrent_Evolution_Class_Access (Driven_Calculation);
      Parent         : Concurrent_Evolution_Class_Access;
      Deepest_Child  : Concurrent_Evolution_Class_Access;
      Perform_Action : Evolution_Action;
      Next_Action    : Evolution_Action;
   begin
      --  exit after evolution (including all subcalculations) has
      --  finished or was canceled
      while Individual /= null loop

         --  Individual might have produced sub-calculations. Continue with
         --  deepest child
         Deepest_Child := Individual;
         while Has_Child (Deepest_Child) loop
            Deepest_Child := Get_Concurrent_Child (Deepest_Child);
         end loop;
         if Deepest_Child /= Individual then
            Individual := Deepest_Child;
            Driver_Controller.Set_Individual (Id, Individual);
         end if;

         --  fetch next action to be performed
         if Driver_Controller.Is_Canceled (Id) then
            Perform_Action := Cancel;
         else
            Perform_Action := Get_Next_Action (Individual);
         end if;

         --  perform the action
         if Perform_Action = Run then
            Step (Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Individual, Next_Action);
         else
            --  need synchronization
            Driver_Controller.Schedule_Synchronization (Id);
            accept Synchronize do
               --  Cancel might have been requested while waiting for
               --  entry call --> poll again
               if Driver_Controller.Is_Canceled (Id) then
                  Perform_Action := Cancel;
               end if;

               case Perform_Action is
                  when Synchronize =>
                     Synchronized_Step (Individual, Next_Action);
                     Set_Next_Action (Individual, Next_Action);

                  when Finish =>
                     --  Fetch parent first, because 'Individual' must not
                     --  be accessed after the call to 'Done'
                     Parent := Get_Concurrent_Parent (Individual);
                     Done (Individual, False);
                     --  Finish 'Individual' and continue with parent
                     --  if no parent exists then task terminates
                     Individual := Parent;
                     if Individual /= null then
                        Driver_Controller.Set_Individual (Id, Individual);
                     else
                        Driver_Controller.Driver_Died (Id);
                     end if;

                  when Cancel =>
                     loop
                        --  Fetch parent first, because 'Individual' must not
                        --  be accessed after the call to 'Done'
                        Parent := Get_Concurrent_Parent (Individual);
                        Done (Individual, True);
                        --  Finish 'Individual' and continue with parent
                        --  if no parent exists then task terminates
                        Individual := Parent;
                        exit when Individual = null;
                     end loop;
                     Driver_Controller.Driver_Died (Id);

                  when others =>
                     --  cannot happen
                     pragma Assert (False);
                     null;

               end case;
            end Synchronize;
         end if;
      end loop;
   end Concurrent_Driver;


   -----------------------
   -- Driver priorities --
   -----------------------

   function Has_Higher_Priority
     (Left  : in Driver_State_Type;
      Right : in Driver_State_Type)
     return Boolean is
   begin
      if Left.Current_State = Right.Current_State then
         return Ada.Real_Time."<" (Left.State_Change, Right.State_Change);
      else
         return Left.Current_State < Right.Current_State;
      end if;
   end Has_Higher_Priority;


   --------------------------------------
   -- Driver_Controller implementation --
   --------------------------------------

   --  All managed Drivers
   Drivers : Driver_Array :=
     (others => No_Driver_State);


   function Has_Higher_Priority_Id
     (Left  : in Driver_Id_Type;
      Right : in Driver_Id_Type)
     return Boolean is
   begin
      return Has_Higher_Priority (Drivers (Left), Drivers (Right));
   end Has_Higher_Priority_Id;

   procedure Set_Position
     (Driver_Id : in     Driver_Id_Type;
      Position  : in     Natural) is
   begin
      Drivers (Driver_Id).Position := Position;
   end Set_Position;

   function Get_Position
     (Driver_Id : in Driver_Id_Type)
     return Natural is
   begin
      return Drivers (Driver_Id).Position;
   end Get_Position;


   --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
   procedure Begin_Concurrent_Updates;

   --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
   procedure End_Concurrent_Updates;


   protected body Driver_Controller is

      procedure Set_State
        (Driver_Id : in     Driver_Id_Type;
         State     : in     Driver_Action_Type) is
      begin
         Drivers (Driver_Id).Current_State := State;
         Drivers (Driver_Id).State_Change := Ada.Real_Time.Clock;
         Driver_State_Queues.Update_Item (Update_Queue, Driver_Id);
      end Set_State;

      --  must be run between Gdk.Threads.Enter and Gdk.Thread.Leave.
      procedure Initialize_Driver
        (Driver_Id       : in     Driver_Id_Type;
         Individual      : access Concurrent_Evolution'Class;
         Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog;
         Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar;
         Progress_Text   : in     Gtk.Label.Gtk_Label;
         Progress_Cancel : in     Gtk.Button.Gtk_Button) is
      begin
         Drivers (Driver_Id) :=
           (Driver          => new Concurrent_Driver
                                     (Driver_Id, Individual),
            Individual      => Concurrent_Evolution_Class_Access (Individual),
            Current_State   => Running,
            State_Change    => Ada.Real_Time.Clock,
            Cancel_Request  => False,
            Position        => 0,
            Cancel_Handler  => 0,
            Progress_Dialog => Progress_Dialog,
            Progress_Bar    => Progress_Bar,
            Progress_Text   => Progress_Text,
            Progress_Cancel => Progress_Cancel);

         if Driver_State_Queues.Is_Empty (Update_Queue) then
            Begin_Concurrent_Updates;
         end if;
         Driver_State_Queues.Insert (Update_Queue, Driver_Id);

         if Gtk.Progress_Bar."/=" (Progress_Bar, null) then
            Gtk.Progress_Bar.Ref (Progress_Bar);
         end if;
         if Gtk.Label."/=" (Progress_Text, null) then
            Gtk.Label.Ref (Progress_Text);
         end if;
         if Gtk.Button."/=" (Progress_Cancel, null) then
            Gtk.Button.Ref (Progress_Cancel);
            Button_Mappings.Bind
              (Cancel_Mapping, Progress_Cancel, Driver_Id);
            Drivers (Driver_Id).Cancel_Handler := Button_Cb.Connect
              (Widget => Progress_Cancel,
               Name   => "clicked",
               Marsh  => Button_Cb.To_Marshaller
               (Cancel_Callback'Access));
         end if;
         if Gtk.Dialog."/=" (Progress_Dialog, null) then
            Gtk.Dialog.Ref (Progress_Dialog);
            Gtk.Dialog.Show_All (Progress_Dialog);
         end if;
      end Initialize_Driver;


      --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Shutdown_Driver
        (Driver_Id : in     Driver_Id_Type) is

         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Concurrent_Driver,
            Name   => Concurrent_Driver_Access);

      begin
         Driver_State_Queues.Remove_Item (Update_Queue, Driver_Id);
         if Driver_State_Queues.Is_Empty (Update_Queue) then
            End_Concurrent_Updates;
         end if;

         Free (Drivers (Driver_Id).Driver);

         --  Drivers (Driver_Id).Individual must not be touched

         if Gtk.Progress_Bar."/=" (Drivers (Driver_Id).Progress_Bar, null) then
            Gtk.Progress_Bar.Unref (Drivers (Driver_Id).Progress_Bar);
         end if;
         if Gtk.Label."/=" (Drivers (Driver_Id).Progress_Text, null) then
            Gtk.Label.Unref (Drivers (Driver_Id).Progress_Text);
         end if;
         if Gtk.Button."/=" (Drivers (Driver_Id).Progress_Cancel, null) then
            Gtk.Handlers.Disconnect
              (Object => Drivers (Driver_Id).Progress_Cancel,
               Id     => Drivers (Driver_Id).Cancel_Handler);
            Button_Mappings.Unbind
              (Cancel_Mapping, Drivers (Driver_Id).Progress_Cancel);
            Gtk.Button.Unref (Drivers (Driver_Id).Progress_Cancel);
         end if;
         if Gtk.Dialog."/=" (Drivers (Driver_Id).Progress_Dialog, null) then
            Gtk.Dialog.Destroy (Drivers (Driver_Id).Progress_Dialog);
            Gtk.Dialog.Unref (Drivers (Driver_Id).Progress_Dialog);
         end if;

         Drivers (Driver_Id) := No_Driver_State;
      end Shutdown_Driver;


      --  must be run between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Update_Visuals
        (Driver_Id : in     Driver_Id_Type) is
      begin
         Update_Visuals
           (Individual    => Drivers (Driver_Id).Individual,
            Progress_Bar  => Drivers (Driver_Id).Progress_Bar,
            Progress_Text => Drivers (Driver_Id).Progress_Text);
      end Update_Visuals;


      --  must be called between Gdk.Thread.Enter and Gdk.Threads.Leave
      procedure Start_Driver
        (Individual      : access Concurrent_Evolution'Class;
         Started         :    out Boolean;
         Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog;
         Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar;
         Progress_Text   : in     Gtk.Label.Gtk_Label;
         Progress_Cancel : in     Gtk.Button.Gtk_Button) is

         Current : Driver_Id_Type := Drivers'First;
      begin
         Started := False;
         if Current <= Drivers'Last then
            loop
               Started := Drivers (Current) = No_Driver_State;
               exit when Started or else Current = Drivers'Last;
               Current := Current + 1;
            end loop;
            if Started then
               Initialize_Driver
                 (Current, Individual, Progress_Dialog,
                  Progress_Bar, Progress_Text, Progress_Cancel);
            end if;
         end if;
      end Start_Driver;

      -- must be called between Gdk.Threads.Enter and Gdk.Threads.Leave
      procedure Perform_Update
        (Must_Synchronize :    out Concurrent_Driver_Access) is

         Driver_Id : Driver_Id_Type;
      begin
         Must_Synchronize := null;

         if not Driver_State_Queues.Is_Empty (Update_Queue) then
            Driver_Id := Driver_State_Queues.Get_Head (Update_Queue);

            --  Set_State must be called to assign a new time-stamp and
            --  thus set to correct position in 'Update_Queue'
            case Drivers (Driver_Id).Current_State is

               when Waiting_On_Sync =>
                  Update_Visuals (Driver_Id);
                  Must_Synchronize := Drivers (Driver_Id).Driver;
                  Set_State (Driver_Id, Running);

               when Dead =>
                  --  must not update visuals because 'Individual' must not
                  --  be accessed
                  if Drivers (Driver_Id).Driver'Terminated then
                     Shutdown_Driver (Driver_Id);
                  else
                     Set_State (Driver_Id, Dead);
                  end if;

               when Running =>
                  Update_Visuals (Driver_Id);
                  Set_State (Driver_Id, Running);
            end case;
         end if;
      end Perform_Update;

      entry Set_Individual
        (Driver_Id       : in     Driver_Id_Type;
         Individual      : access Concurrent_Evolution'Class) when True is
      begin
         Drivers (Driver_Id).Individual :=
           Concurrent_Evolution_Class_Access (Individual);
      end Set_Individual;

      entry Schedule_Synchronization
        (Driver_Id       : in     Driver_Id_Type) when True is
      begin
         Set_State (Driver_Id, Waiting_On_Sync);
      end Schedule_Synchronization;

      entry Driver_Died
        (Driver_Id       : in     Driver_Id_Type) when True is
      begin
         Drivers (Driver_Id).Individual := null;
         Set_State (Driver_Id, Dead);
      end Driver_Died;

      procedure Schedule_Cancel
        (Progress_Cancel : in     Gtk.Button.Gtk_Button) is

         Driver_Id : Driver_Id_Type;
      begin
         Driver_Id := Button_Mappings.Fetch (Cancel_Mapping, Progress_Cancel);
         Drivers (Driver_Id).Cancel_Request := True;
         --  does not affect priority!
      end Schedule_Cancel;

      function Is_Canceled
        (Driver_Id       : in     Driver_Id_Type)
        return Boolean is
      begin
         return Drivers (Driver_Id).Cancel_Request;
      end Is_Canceled;

   end Driver_Controller;


   ---------------------------------
   -- Concurrent Driver Callbacks --
   ---------------------------------

   procedure Cancel_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Driver_Controller.Schedule_Cancel (Gtk.Button.Gtk_Button (Button));
   end Cancel_Callback;


   function Concurrent_Update_Callback
     return Boolean is

      Driver_To_Sync : Concurrent_Driver_Access;
   begin
      Gdk.Threads.Enter;

      Driver_Controller.Perform_Update (Driver_To_Sync);
      if Driver_To_Sync /= null then
         Driver_To_Sync.Synchronize;
      end if;

      Gdk.Main.Flush;
      Gdk.Threads.Leave;
      return True;
   end Concurrent_Update_Callback;



   --------------------------------
   -- Iterative Driver Callbacks --
   --------------------------------

   Driver_State : Iterative_Driver_State_Type := No_Iterative_Driver_State;


   procedure Iterative_Cancel_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Driver_State.Cancel_Request := True;
   end Iterative_Cancel_Callback;


   function Iterative_Driver_Callback
     return Boolean is

      Parent         : Evolution_Class_Access;
      Child          : Evolution_Class_Access;
      Perform_Action : Evolution_Action;
      Next_Action    : Evolution_Action;
   begin
      --  Individual might have produced sub-calculations. Continue with
      --  deepest child
      loop
         Child := Get_Child (Driver_State.Individual);
         exit when Child = null;
         Driver_State.Individual := Child;
      end loop;

      --  Update visuals if time has come
      if Ada.Real_Time.">="
        (Left  => Ada.Real_Time.Clock,
         Right => Ada.Real_Time."+"
                    (Driver_State.Update_Time,
                     Ada.Real_Time.Milliseconds (Poll_Delay_Milli_Seconds)))
      then
         Update_Visuals
           (Driver_State.Individual,
            Driver_State.Progress_Bar,
            Driver_State.Progress_Text);
         Driver_State.Update_Time := Ada.Real_Time.Clock;
      end if;

      --  fetch next action to be performed
      if Driver_State.Cancel_Request then
         Perform_Action := Cancel;
      else
         Perform_Action := Get_Next_Action (Driver_State.Individual);
      end if;

      --  perform the action
      case Perform_Action is
         when Run =>
            Step (Driver_State.Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Driver_State.Individual, Next_Action);

         when Synchronize =>
            Synchronized_Step (Driver_State.Individual, Next_Action);
            --  memorize next action to be performed on 'Individual'
            Set_Next_Action (Driver_State.Individual, Next_Action);

         when Finish =>
            --  Fetch parent first, because 'Individual' must not
            --  be accessed after the call to 'Done'
            Parent := Get_Parent (Driver_State.Individual);
            Done (Driver_State.Individual, False);
            --  Finish 'Individual' and continue with parent
            --  if no parent exists then terminate Driver
            Driver_State.Individual := Parent;

         when Cancel =>
            loop
               --  Fetch parent first, because 'Individual' must not
               --  be accessed after the call to 'Done'
               Parent := Get_Parent (Driver_State.Individual);
               Done (Driver_State.Individual, True);
               --  Finish 'Individual' and continue with parent
               --  if no parent exists then terminate Driver
               Driver_State.Individual := Parent;
               exit when Driver_State.Individual = null;
            end loop;
      end case;

      if Driver_State.Individual = null then
         --  terminate Driver
         Stop_Iterative_Driver;
         Driver_State := No_Iterative_Driver_State;
         return False;
      else
         --  continue on next idle signal
         return True;
      end if;
   end Iterative_Driver_Callback;


   -----------------------
   -- Callback-handling --
   -----------------------

   Concurrent_Update_Id : Gtk.Main.Timeout_Handler_Id;

   procedure Begin_Concurrent_Updates is
   begin
      Concurrent_Update_Id := Gtk.Main.Timeout_Add
        (Poll_Delay_Milli_Seconds, Concurrent_Update_Callback'Access);
   end Begin_Concurrent_Updates;

   procedure End_Concurrent_Updates is
   begin
      Gtk.Main.Timeout_Remove (Concurrent_Update_Id);
   end End_Concurrent_Updates;


   procedure Start_Iterative_Driver
     (Individual      : access Iterative_Evolution'Class;
      Started         :    out Boolean;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar;
      Progress_Text   : in     Gtk.Label.Gtk_Label;
      Progress_Cancel : in     Gtk.Button.Gtk_Button) is

      Idle_Handler : Gtk.Main.Idle_Handler_Id;
   begin
      Started := Driver_State = No_Iterative_Driver_State;
      if Started then
         Driver_State :=
           (Individual      => Evolution_Class_Access (Individual),
            Update_Time     => Ada.Real_Time.Clock,
            Cancel_Request  => False,
            Cancel_Handler  => 0,
            Progress_Dialog => Progress_Dialog,
            Progress_Bar    => Progress_Bar,
            Progress_Text   => Progress_Text,
            Progress_Cancel => Progress_Cancel);

         if Gtk.Progress_Bar."/=" (Driver_State.Progress_Bar, null) then
            Gtk.Progress_Bar.Ref (Driver_State.Progress_Bar);
         end if;
         if Gtk.Label."/=" (Driver_State.Progress_Text, null) then
            Gtk.Label.Ref (Driver_State.Progress_Text);
         end if;
         if Gtk.Button."/=" (Driver_State.Progress_Cancel, null) then
            Gtk.Button.Ref (Driver_State.Progress_Cancel);
            Driver_State.Cancel_Handler := Button_Cb.Connect
              (Widget  => Driver_State.Progress_Cancel,
               Name    => "clicked",
               Marsh   => Button_Cb.To_Marshaller
                            (Iterative_Cancel_Callback'Access));
         end if;
         if Gtk.Dialog."/=" (Driver_State.Progress_Dialog, null) then
            Gtk.Dialog.Ref (Driver_State.Progress_Dialog);
            Gtk.Dialog.Set_Modal (Driver_State.Progress_Dialog);
            Gtk.Dialog.Show_All (Driver_State.Progress_Dialog);
         end if;
         Idle_Handler := Gtk.Main.Idle_Add
           (Iterative_Driver_Callback'Access);
      end if;
   end Start_Iterative_Driver;

   procedure Stop_Iterative_Driver is
   begin
      if Gtk.Progress_Bar."/=" (Driver_State.Progress_Bar, null) then
         Gtk.Progress_Bar.Unref (Driver_State.Progress_Bar);
      end if;
      if Gtk.Label."/=" (Driver_State.Progress_Text, null) then
         Gtk.Label.Unref (Driver_State.Progress_Text);
      end if;
      if Gtk.Button."/=" (Driver_State.Progress_Cancel, null) then
         Gtk.Handlers.Disconnect
           (Object => Driver_State.Progress_Cancel,
            Id     => Driver_State.Cancel_Handler);
         Gtk.Button.Unref (Driver_State.Progress_Cancel);
      end if;
      if Gtk.Dialog."/=" (Driver_State.Progress_Dialog, null) then
         Gtk.Dialog.Destroy (Driver_State.Progress_Dialog);
         Gtk.Dialog.Unref (Driver_State.Progress_Dialog);
      end if;
      Driver_State := No_Iterative_Driver_State;
   end Stop_Iterative_Driver;

end Giant.Evolutions;
