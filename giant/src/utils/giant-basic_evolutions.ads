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
--  $RCSfile: giant-basic_evolutions.ads,v $, $Revision: 1.4 $
--  $Author: keulsn $
--  $Date: 2003/09/12 20:30:12 $
--
------------------------------------------------------------------------------
--
--  This package provides a basic framework for user-cancelable operations.
--
--  See Giant.Evolutions for more details.
--

with Ada.Real_Time;

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Giant.Progress_Dialog;

package Giant.Basic_Evolutions is

   type Basic_Evolution is private;

   type Basic_Evolution_Access is access Basic_Evolution;

   function Create
     (Dialog : in Progress_Dialog.Progress_Dialog_Access)
     return Basic_Evolution_Access;

   procedure Destroy
     (Individual : in out Basic_Evolution_Access);

   ---------------------------------------------------------------------------
   --  Advances the
   --
   --  Parameters:
   --    Individual  - Subject of the evolution process
   --    Next_Action - Determines the action to be performed next
   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
     return Boolean;

   procedure Set_Cancel_Enabled
     (Individual : in Basic_Evolution_Access;
      Enabled    : in Boolean);

   function Set_Percentage
     (Individual : in Basic_Evolution_Access;
      Percentage : in Float;
      Text       : in String := "")
     return Boolean;

   ----------------------------------------------------------------------------
   --  Sets a format string to be displayed in a Progress_Dialog if the
   --  complexity of 'Individual' is known. Should be overwritten by
   --  descendants to show a more meaningful text.
   --
   --  MUST be able to be reentrant from different tasks. Usually should
   --  return a constant String
   --
   --  The following Texts can be replaced inside the String:
   --    %v - Natural'Image (Get_Progress_Count (Individual))
   --    %u - Natural'Image (Get_Complexity (Individual))
   --    %p - current progress percentage
   --
   --  Precondition
   --    Get_Complexity (Individual) > 0
   --  Returns:
   --    "%v " & (-"of") & " %u"
   procedure Set_Text
     (Individual : in Basic_Evolution_Access;
      Text       : in String);

   ---------------------------------------------------------------------------
   --  Sets the complexity of this 'Individuals's evolution to 'Total_Number'
   --  items. If the complexity is unknown, this procedure should either
   --  never be called or 0 should be set.
   --
   --  If the complexity is set to a value different to 0 then the complexity
   --  should at any given point of time be greater or equal to the progress
   --  counter accessed through 'Advance_Progress'.
   --
   --  The complexity is interrogated from a multi-tasking environment without
   --  synchronization. Each Individual must through the order of calls to
   --  'Set_Complexity' and 'Advance_Progress' ensure that the combination of
   --  progress counter and complexity is meaningful to the user at any
   --  point of time.
   --
   --  Parameters
   --    Individual   - Subject of the evolution process
   --    Total_Number - Number of items processed when this evolution has
   --                   finished, or 0 if unknown
   procedure Set_Total_Complexity
     (Individual       : in Basic_Evolution_Access;
      Total_Complexity : in Natural);

private

   function Iterate_Main
     (Individual   : in Basic_Evolution_Access;
      Force_Update : in Boolean)
     return Boolean;

   type Basic_Evolution is
      record
         --  A monitoring dialog or null
         Dialog              : Progress_Dialog.Progress_Dialog_Access;
         Cancel_Handler      : Gtk.Handlers.Handler_Id;
         Cancelled           : Boolean := False;
         Complexity          : Natural := 0;
         Last_Main_Iteration : Ada.Real_Time.Time;
      end record;

end Giant.Basic_Evolutions;
