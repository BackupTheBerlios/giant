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
--  $RCSfile: giant-set_operation_dialog.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
--
--

with Gtk.Combo;
with Gtk.Enums;
with Gtk.Gentry;

with Giant.Default_Dialog;

package Giant.Set_Operation_Dialog is

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Set_Operation_Dialog_Access is
     access all Set_Operation_Dialog_Record'Class;

   type Operation_Type is (Difference, Intersection, Union);

   Invalid_Operation_Entered : exception;

   procedure Create
     (Dialog :    out Set_Operation_Dialog_Access;
      Items  : in     Gtk.Enums.String_List.Glist);

   procedure Initialize
     (Dialog : access Set_Operation_Dialog_Record'Class;
      Items  : in     Gtk.Enums.String_List.Glist);

   function Get_Left_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Right_Source
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Get_Operation
     (Dialog : access Set_Operation_Dialog_Record)
     return Operation_Type;

   function Get_Target
     (Dialog : access Set_Operation_Dialog_Record)
     return String;

   function Validate
     (Dialog : access Set_Operation_Dialog_Record)
     return Boolean;

private

   type Set_Operation_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Left_Source : Gtk.Combo.Gtk_Combo;
        Right_Source : Gtk.Combo.Gtk_Combo;
        Operation : Gtk.Combo.Gtk_Combo;
        Target : Gtk.Gentry.Gtk_Entry;
     end record;

end Giant.Set_Operation_Dialog;
