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
--  $RCSfile: giant-basic_evolutions.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
--  This package provides a basic framework for user-cancelable
--  operations. The main difference to the Giant.Evolutions package is
--  that the control flow remains within the operation.
--
--  The operation periodically needs to call Step() or Set_Percentage().
--
--  All procedures except for Create and Destroy can be called with
--  Individual = null. In that case no operation is performed.
--
--  See:
--    Giant.Evolutions

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
   --  Advances the operation by Delta_Complexity. If enough time has
   --  gone by, the progress dialog is updated accordingly.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Delta_Complexity - The operation is advanced this much
   --  Returns:
   --    True, if the operation was cancelled
   --  See:
   --    Set_Total_Complexity
   function Step
     (Individual       : in Basic_Evolution_Access;
      Delta_Complexity : in Natural                := 1)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Enables or disables the cancel button.
   procedure Set_Cancel_Enabled
     (Individual : in Basic_Evolution_Access;
      Enabled    : in Boolean);

   ---------------------------------------------------------------------------
   --  Advances the progress to Percentage and updates the progress
   --  dialog.
   --
   --  Parameters:
   --    Individual - Subject of the evolution process
   --    Percentage - A value between 0.0 and 1.0.
   --    Text - The text to display on the progress bar.
   --  Returns:
   --    True, if the operation was cancelled
   function Set_Percentage
     (Individual : in Basic_Evolution_Access;
      Percentage : in Float;
      Text       : in String := "")
     return Boolean;

   ----------------------------------------------------------------------------
   --  Sets a format string to be displayed on the progress bar.
   --
   --  The following Texts can be replaced inside the String:
   --    %v - current complexity
   --    %u - total complexity
   --    %p - current progress percentage
   procedure Set_Text
     (Individual : in Basic_Evolution_Access;
      Text       : in String);

   ---------------------------------------------------------------------------
   --  Sets the total complexity of the operation. If complexity is
   --  unknown 0 should be passed to set the progress dialog to
   --  activity mode.
   --
   --  Parameters
   --    Individual - Subject of the evolution process
   --    Total_Complexity - Number of items processed when this evolution has
   --                       finished, or 0 if unknown
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
