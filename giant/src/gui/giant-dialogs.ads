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
--  $RCSfile: giant-dialogs.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:54 $
--
------------------------------------------------------------------------------
--
-- Provides common dialogs.
--

with Gtk.Object;

with Giant.Default_Dialog;
with Giant.Input_Dialog;
pragma Elaborate (Giant.Input_Dialog);

package Giant.Dialogs is

   package Gtk_Object_Input_Dialog is new Giant.Input_Dialog
     (Gtk.Object.Gtk_Object);

   function Show_Confirmation_Dialog
     (Message : in String;
      Buttons : in Default_Dialog.Button_Type := Default_Dialog.Button_Yes_No)
      return Default_Dialog.Response_Type;

   function Show_Delete_Confirmation_Dialog
     (Message : in String := -"Do you really want to delete?")
     return Boolean;

   procedure Show_Error_Dialog
     (Message : in String;
      Title   : in String := -"Giant Error");

   function Show_Input_Dialog
     (Message         : in String;
      Title           : in String                                   := -"Giant Input";
      Default_Input   : in String                                   := "";
      Input_Validator : in Gtk_Object_Input_Dialog.Input_Validator_Type := null)

     return String;

end Giant.Dialogs;
