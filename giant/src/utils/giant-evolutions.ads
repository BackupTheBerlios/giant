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
--  $RCSfile: giant-evolutions.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/05/23 16:39:04 $
--
------------------------------------------------------------------------------
--
--  This package provides a framework for user-cancelable calculations.
--
--  Each such calculation is considered an Individual that is created in
--  an initial state, then undergoes a certain evolution process until
--  the calculation is completed. During the evolution process, a dialog
--  box is shown to the user. In that box the progress of the Evolution
--  is reported to the user. Inside the box the user can hit the
--  cancel-button in order to stop the evolution before it has finished.
--
--  Every Individual is represented as an instance of any subclass of the
--  tagged type 'Evolution'. The evolution process is modeled by subsequent
--  calls to the primitive subprogram 'Step' or 'Step_Synchronized'.
--
--  Two different types of evolutions are distinguished:
--  * 'Concurrent_Evolution'
--    Individuals of that class can run in a task parallel to the GtkAda
--    main task. During normal running they must not execute any action
--    that might affect other tasks. From time to time these Individuals
--    can perform an action that is synchronized within the GtkAda main
--    task.
--  * 'Iterative_Evolution'
--    Individuals of that class are run within the GtkAda main task. They
--    need not care about synchronization.
--    The dialog box is modal so no other function of the system should be
--    available. Functions accessible by other means than GtkAda must be
--    locked explicitely. The Iterative_Evolution is interrupted after each
--    step and the control resource is given back to GtkAda. Thus GtkAda
--    is still able to receive the cancel hit-event.
--

with Gtk.Dialog;
with Gtk.Label;
with Gtk.Progress_Bar;

package Giant.Evolutions is

   ---------------------------------------------------------------------------
   --  Maximum Number of slots. Each evolution processe uses one slot.
   --  Thus the number of concurrently running evolutions is limited by this
   --  number.
   Number_Of_Slots          : constant :=  10;

   ---------------------------------------------------------------------------
   --  Delay time between two updates of an progress window. At each update
   --  only one window will be updated, so each process has a delay time
   --  of this constant multiplyed by the number of running processes.
   Poll_Delay_Milli_Seconds : constant := 500;

   ---------------------------------------------------------------------------
   --  All types of atomic operations of an Evolution
   --  * 'Run':         Perform the calculation. 'Step' will be called.
   --  * 'Synchronize': Perform an action within a synchronized environment
   --                   (only applies to instances of 'Concurrent_Evolution'
   --  * 'Cancel':      Stop the Calculation, 'Finish' will be called.
   --  * 'Finish':      Calculation complete, 'Finish' will be called.
   type Evolution_Action is (Run, Synchronize, Cancel, Finish);


   --------------------------------------
   -- Tagged type Evolution (abstract) --
   --------------------------------------

   ---------------------------------------------------------------------------
   --  Evolution base type. Do not subclass this type directly! Use
   --  'Concurrent_Evolution' or 'Iterative_Evolution'.
   type Evolution is abstract tagged limited private;

   type Evolution_Class_Access is access all Evolution'Class;

   ---------------------------------------------------------------------------
   --  Initializes 'Individual'. Must be called by children of this tagged
   --  type.
   --
   --  Parameters:
   --    Individual - Individual to be initialized
   --    Complexity - Estimate of the complexity of this calculation or 0
   --                 if a sensible estimate is not available.
   procedure Initialize
     (Individual : access Evolution;
      Complexity : in     Natural := 0);

   ---------------------------------------------------------------------------
   --  Performs one atomic step in 'Individual's calculation. Before and
   --  after this step 'Individual' is in a state in which it can be
   --  finished by a call to 'Finish'.
   --
   --  Depending on the actual type of Individual more constraints are
   --  specified. See description of type 'Concurrent_Evolution' and
   --  'Iterative_Evolution' for details.
   --
   --  Step must set 'Next_Action' to the desired value. See type
   --  'Evolution_Action' for reference.
   --
   --  !This procedure should not be called from outside this package!
   --
   --  Parameters:
   --    Individual  - Subject of the evolution process
   --    Next_Action - Determines the action to be performed next
   procedure Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action) is abstract;

   ---------------------------------------------------------------------------
   --  Performs one atomic step in 'Individual's calculation. Before and
   --  after this step 'Individual' is in a state in which it can be
   --  finished by a call to 'Finish'. In difference to the procedure 'Step'
   --  this procedure is always executed inside a task synchronized with
   --  GtkAda.
   --
   --  An Individual must apply for a call to this procedure by setting
   --  the value of the out-Parameter 'Next_Action' of the procedure
   --  'Step' to 'Synchronize'. It might take a lot of time until the call
   --  is actually made (up to several seconds). So this procedure should
   --  be used only when really necessary.
   --
   --  'Synchronized_Step' must set 'Next_Action' to the desired value.
   --  See type 'Evolution_Action' for reference.
   --
   --  The default implementation of this procedure does nothing but set
   --  'Next_Action' to 'Run'.
   --
   --  !This procedure should not be called from outside this package!
   --
   --  Parameters:
   --    Individual  - Subject of the evolution process
   --    Next_Action - Determines the action to be performed next
   procedure Synchronized_Step
     (Individual  : access Evolution;
      Next_Action :    out Evolution_Action);

   ---------------------------------------------------------------------------
   --  Will be called after the last call to 'Step' or 'Synchronized_Step'
   --  by this package.
   --
   --  Is run synchronized with Gtk even if is multi-tasking is possible
   --  for 'Individual'. Should free System resources and perform any action
   --  that is necessary to make the calculation's result useful to the
   --  application (eg. release locks, show windows, ...)
   --
   --  May destroy 'Individual'. The access value 'Individual' will not be
   --  used anymore within this package.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Canceled   - True if the Calculation was canceled by user action
   --                 or by 'Individuals's own request.
   --                 False if 'Individual' requested 'Finish'.
   procedure Finish
     (Individual : access Evolution;
      Canceled   : in     Boolean) is abstract;

   ---------------------------------------------------------------------------
   --  Starts the evolution of 'Individual' if enough slots are available
   --  (see 'Number_Of_Slots' above). If not then does nothing. In this
   --  case another call to this function will only be successful after a
   --  different currently running evolution has been finished and cleared
   --  from memory. It may take some time (up to several seconds) after the
   --  call to 'Finish' in one Individual and its removal from memory.
   --
   --  If the evolution starts then the 'Progress_Dialog' will be shown if
   --  one is provided. The 'Progress_Bar' will be updated frequently
   --  if one is provided and the complexity of 'Individual' is known.
   --  the Text in 'Progress_Text' if 'Progress_Text' is provided will be
   --  set to 'Get_Step_Count (Individual)' if the complexity is unknown
   --  or to 'Get_Step_Count (Individual)' "of" 'Get_Complexity (Individual)'
   --  if the complexity is known. If 'Progress_Dialog' is provided then
   --  'Gtk.Dialog.Destroy (Progress_Dialog)' will be called after the
   --  evolution has been canceled or has finished.
   --
   --  This procedure returns immediately (does not wait until the evolution
   --  has finished).
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Progress_Dialog - Dialog that will be destroyed after finish of
   --                      evolution or null
   --    Progress_Bar    - Progress bar that will be updated frequently or
   --                      null
   --    Progress_Text   - Text that will be updated frequently to inform
   --                      the user of the progress or null
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if there are not enough
   --                      resources to start the evolution.
   procedure Start_Calculation
     (Individual      : access Evolution;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog             := null;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : in     Gtk.Label.Gtk_Label               := null;
      Started         :    out Boolean) is abstract;

   ---------------------------------------------------------------------------
   --  Sets the complexity of this 'Individuals's evolution to 'Total_Number'
   --  items. If the complexity is unknown, this procedure should either
   --  never be calles or 0 should be set.
   --
   --  If the complexity is set to a value different to 0 then the complexity
   --  should at any given point of time be greater or equal to the progress
   --  counter accessed through 'Advance_Steps'.
   --
   --  The complexity is interrogated from a multi-tasking environment without
   --  synchronization. Each Individual must through the order of calls to
   --  'Set_Complexity' and 'Advance_Steps' ensure that the combination of
   --  progress counter and complexity is meaningful to the user at any
   --  point of time.
   --
   --  Parameters
   --    Individual   - Subject of the evolution process
   --    Total_Number - Number of items processed when this evolution has
   --                   finished, or 0 if unknown
   procedure Set_Complexity
     (Individual   : access Evolution'Class;
      Total_Number : in     Natural);

   ---------------------------------------------------------------------------
   --  Should only be called inside 'Step' or 'Synchronized_Step'. Increases
   --  the progress counter of this 'Individual' by 'Progress'.
   --
   --  Parameters:
   --    Individual - Subject of the evolution progress
   --    Progress   - Number of items recently processed
   procedure Advance_Steps
     (Individual : access Evolution'Class;
      Progress   : in     Natural);

   ---------------------------------------------------------------------------
   --  Returns the number of processed items stored in the progress counter.
   --  Initially the progress counter is 0 and will be increased during
   --  calculation through calls to 'Advance_Steps'.
   function Get_Step_Count
     (Evolver      : access Evolution'Class)
      return Natural;

   ---------------------------------------------------------------------------
   --  Returns the total complexity of the calculation measured in number
   --  of items to be processed. If the complexity is unknown, than this
   --  function returns 0.
   --  If the complexity is non-zero then the calculation might finish
   --  when 'Get_Step_Counter = Get_Complexity' although this is NOT assured.
   function Get_Complexity
     (Evolver      : access Evolution'Class)
      return Natural;


   -------------------------------------------------
   -- Tagged type Concurrent_Evolution (abstract) --
   -- derived from Evolution                      --
   -------------------------------------------------

   ---------------------------------------------------------------------------
   --  Individuals of this class can be run in a multi-tasking environment.
   --  They affect other concurrently executed tasks only in defined and
   --  desired ways.
   type Concurrent_Evolution is abstract new Evolution with private;

   type Concurrent_Evolution_Class_Access is
     access all Concurrent_Evolution'Class;

   ---------------------------------------------------------------------------
   --  procedure Step
   --    (Individual  : access Evolution;
   --     Next_Action : in     Evolution_Action) is abstract;
   --
   --  See description of type 'Evolution' for details. Only the constraints
   --  specific to the type 'Concurrent_Evolution' are stated here:
   --
   --  Since 'Individual' can be used in a multi-tasking environment,
   --  the execution of this procedure must not affect other tasks.
   --  This procedure is NOT synchronized with GtkAda.

   ---------------------------------------------------------------------------
   --  Derived from 'Evolution'. Starts an 'Concurrent_Evolution' in
   --  multi-tasking mode.
   --
   --  This procedure returns immediately (does not wait until the evolution
   --  has finished).
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Progress_Dialog - Dialog that will be destroyed after finish of
   --                      evolution or null
   --    Progress_Bar    - Progress bar that will be updated frequently or
   --                      null
   --    Progress_Text   - Text that will be updated frequently to inform
   --                      the user of the progress or null
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if there are not enough
   --                      resources to start the evolution.
   procedure Start_Calculation
     (Individual      : access Concurrent_Evolution;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog             := null;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : in     Gtk.Label.Gtk_Label               := null;
      Started         :    out Boolean);

   ---------------------------------------------------------------------------
   --  Freezes 'Master's evolution process and starts evolution of
   --  'Sub_Calculation'. Uses the same progress dialog used for 'Master'
   --  If 'Sub_Calculation' gets canceled, then the Master will be canceled,
   --  too. If 'Sub_Calculation' finishes, then the Master will be woken up
   --  and continue its evolution process.
   --
   --  Since Sub_Calculation can reuse the 'Master's slot, this procedure
   --  never fails in contrast to 'Start_Calculation'. This procedure
   --  returns immediately, without waiting for the evolution of
   --  'Sub_Calculation' to finish.
   --
   --  'Sub_Calculation' must be derived from 'Concurrent_Evolution' and is
   --  thus able to be run in multi-tasking mode.
   --  There is another 'Start_Sub_Calculation' for 'Iterative_Evolution'
   --  as well.
   --
   --  Parameters:
   --    Master          - An Individual that is currently evolving.
   --    Sub_Calculation - An Individual that should be evolved before
   --                      'Master' can continue its evolution
   --  Precondition:
   --    'Master's evolution has been started by a call to
   --    'Start_Calculation' and 'Start_Calculation' has set the 'Started'
   --    Argument to True
   --  Postcondition:
   --    * For 'Master' no call to 'Step' or 'Synchronized_Step' will be made
   --      until 'Finish (Sub_Calculation, False)' has been called.
   --    * If 'Finish (Sub_Calculation, True)' is called then
   --      'Finish (Master, True)' will be called as well.
   procedure Start_Sub_Calculation
     (Master          : access Concurrent_Evolution'Class;
      Sub_Calculation : access Concurrent_Evolution'Class);


   ------------------------------------------------
   -- Tagged type Iterative_Evolution (abstract) --
   -- derived from Evolution                     --
   ------------------------------------------------

   ---------------------------------------------------------------------------
   --  Individuals of this class are run inside an idle-event of GtkAda.
   type Iterative_Evolution is abstract new Evolution with private;

   type Iterative_Evolution_Class_Access is
     access all Iterative_Evolution'Class;

   ---------------------------------------------------------------------------
   --  Derived from 'Evolution'. Starts an 'Iterative_Evolution' in
   --  single-tasking mode.
   --
   --  This procedure returns immediately (does not wait until the evolution
   --  has finished).
   --
   --  Parameters:
   --    Individual      - Subject of the evolution process
   --    Progress_Dialog - Dialog that will be destroyed after finish of
   --                      evolution or null
   --    Progress_Bar    - Progress bar that will be updated frequently or
   --                      null
   --    Progress_Text   - Text that will be updated frequently to inform
   --                      the user of the progress or null
   --    Started         - Set to True if the evolution of 'Individual'
   --                      started, set to False if there are not enough
   --                      resources to start the evolution.
   procedure Start_Calculation
     (Individual      : access Iterative_Evolution;
      Progress_Dialog : in     Gtk.Dialog.Gtk_Dialog             := null;
      Progress_Bar    : in     Gtk.Progress_Bar.Gtk_Progress_Bar := null;
      Progress_Text   : in     Gtk.Label.Gtk_Label               := null;
      Started         :    out Boolean);

   ---------------------------------------------------------------------------
   --  Freezes 'Master's evolution process and starts evolution of
   --  'Sub_Calculation'. Uses the same progress dialog used for 'Master'
   --  If 'Sub_Calculation' gets canceled, then the Master will be canceled,
   --  too. If 'Sub_Calculation' finishes, then the Master will be woken up
   --  and continue its evolution process.
   --
   --  Since Sub_Calculation can reuse the 'Master's slot, this procedure
   --  never fails in contrast to 'Start_Calculation'. This procedure
   --  returns immediately, without waiting for the evolution of
   --  'Sub_Calculation' to finish.
   --
   --  'Sub_Calculation' is always run in synchronized mode, even if it was
   --  able to handle multi-tasking.
   --  There is a 'Start_Sub_Calculation' that deals only with
   --  'Concurrent_Evolution's.
   --
   --  Parameters:
   --    Master          - An Individual that is currently evolving.
   --    Sub_Calculation - An Individual that should be evolved before
   --                      'Master' can continue its evolution
   --  Precondition:
   --    'Master's evolution has been started by a call to
   --    'Start_Calculation' and 'Start_Calculation' has set the 'Started'
   --    Argument to True
   --  Postcondition:
   --    * For 'Master' no call to 'Step' or 'Synchronized_Step' will be made
   --      until 'Finish (Sub_Calculation, False)' has been called.
   --    * If 'Finish (Sub_Calculation, True)' is called then
   --      'Finish (Master, True)' will be called as well.
   procedure Start_Sub_Calculation
     (Master          : access Iterative_Evolution'Class;
      Sub_Calculation : access Evolution'Class);


                              ------------------
private                       -- Private Part --
                              ------------------


   ----------------------------------
   -- Type declaration completions --
   ----------------------------------

   type Evolution is abstract tagged limited
      record
         --  Number of items processed
         Progress_Counter : Natural;
         --  Estimate of the maximum value 'Progress_Counter' will ever reach
         --  does not need to be precise, 0 if unknown
         Complexity       : Natural;
      end record;

   type Concurrent_Evolution is abstract new Evolution with null record;

   type Iterative_Evolution is abstract new Evolution with null record;

end Giant.Evolutions;
