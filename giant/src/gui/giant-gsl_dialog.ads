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
--  $RCSfile: giant-gsl_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that can open save and executes GSL scripts.
--

with Ada.Strings.Unbounded;

with Gtk.Status_Bar;
with Gtk.Text;
with Gtk.Window;

with Giant.Default_Dialog;
with Giant.File_Management;
pragma Elaborate_All (Giant.File_Management);

package Giant.Gsl_Dialog is

   type Gsl_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Gsl_Dialog_Access is access all Gsl_Dialog_Record'Class;

   function Can_Hide
     (Dialog : access Gsl_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog : out Gsl_Dialog_Access);

   procedure Initialize
     (Dialog : access Gsl_Dialog_Record'class);

   function Get_Filename
     (Dialog   : access Gsl_Dialog_Record)
     return String;

   procedure Set_Filename
     (Dialog   : access Gsl_Dialog_Record;
      Filename : in     String);

private

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was read successfully; False, otherwise
   function Read
     (Dialog   : access Gsl_Dialog_Record'Class;
      Filename : String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns:
   --    False, if user cancelled or data was not modified; True, otherwise
   function Save_Changes
     (Dialog : access Gsl_Dialog_Record'Class)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Shows a file open dialog and opens the file if the user selects okay.
   --
   --  Parameters:
   --    Delete - If true, the text area is cleared before the file is
   --      inserted
   procedure Show_Open_Dialog
     (Dialog : access Gsl_Dialog_Record'Class;
      Delete : Boolean);

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was saved or dialog was cancelled; False, otherwise
   function Show_Save_As_Dialog
     (Dialog   : access Gsl_Dialog_Record'Class)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns:
   --    True, if file was written successfully; False, otherwise
   function Write
     (Dialog   : access Gsl_Dialog_Record'Class;
      Filename : String)
     return Boolean;

   --  If no filename is set this file will be used
   Temp_Gsl_Filename : constant String
     := File_Management.Get_User_Config_Path & "temp.gsl";

   type Gsl_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Filename : Ada.Strings.Unbounded.Unbounded_String
          := Ada.Strings.Unbounded.Null_Unbounded_String;
        Text_Area : Gtk.Text.Gtk_Text;
        Text_Has_Changed : Boolean := False;
        Filename_Bar : Gtk.Status_Bar.Gtk_Status_Bar;
        Location_Bar : Gtk.Status_Bar.Gtk_Status_Bar;
        Last_Save_Date : File_Management.OS_Time;
     end record;

end Giant.Gsl_Dialog;
