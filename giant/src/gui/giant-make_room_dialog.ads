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
--  $RCSfile: giant-make_room_dialog.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
-- Provides a dialog that provides a spin button.
--

with Glib;
with Gtk.Adjustment;

with Giant.Default_Dialog;

package Giant.Make_Room_Dialog is

   DEFAULT_VALUE : constant Glib.Gdouble := 30.0;

   type Make_Room_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with private;

   type Make_Room_Dialog_Access is
      access all Make_Room_Dialog_Record'Class;

   function Can_Hide
     (Dialog : access Make_Room_Dialog_Record)
     return Boolean;

   procedure Create
     (Dialog :    out Make_Room_Dialog_Access);

   procedure Initialize
     (Dialog : access Make_Room_Dialog_Record'class);

   function Show
     return Float;

private
   type Make_Room_Dialog_Record is
     new Default_Dialog.Default_Dialog_Record with record
        Pixel_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
     end record;

end Giant.Make_Room_Dialog;
