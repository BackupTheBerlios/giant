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
--  $RCSfile: giant-selection_operation_dialog.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--
------------------------------------------------------------------------------
--
--
--

with Ada.Strings.Unbounded;

with Giant.Set_Operation_Dialog;

package Giant.Selection_Operation_Dialog is

   type Selection_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with private;

   type Selection_Operation_Dialog_Access is
     access all Selection_Operation_Dialog_Record'Class;

   procedure Create
     (Dialog	  :    out Selection_Operation_Dialog_Access;
	  Window_Name : in     String);

   procedure Initialize
     (Dialog	  : access Selection_Operation_Dialog_Record'Class;
	  Window_Name : in     String);
   
   function Can_Hide
     (Dialog : access Selection_Operation_Dialog_Record)
     return Boolean;

private
   
   type Selection_Operation_Dialog_Record is
     new Set_Operation_Dialog.Set_Operation_Dialog_Record with record
		Window_Name : Ada.Strings.Unbounded.Unbounded_String;
	 end record;
   

end Giant.Selection_Operation_Dialog;
