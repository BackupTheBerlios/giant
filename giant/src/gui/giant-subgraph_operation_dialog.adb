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
--  $RCSfile: giant-subgraph_operation_dialog.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Ada.Strings.Unbounded;

with Gtk.Combo;
with Gtk.Enums;
with Gtk.Gentry;

with String_Lists;

with Giant.Controller;
with Giant.Default_Dialog;
with Giant.Projects;

package body Giant.Subgraph_Operation_Dialog is

   ---------------------------------------------------------------------------
   --  Callbacks
   ---------------------------------------------------------------------------

   function Can_Hide
     (Dialog : access Subgraph_Operation_Dialog_Record)
     return Boolean
   is
      use type Default_Dialog.Response_Type;
      use type Set_Operation_Dialog.Operation_Type;
	  
	  Operation : Set_Operation_Dialog.Operation_Type;
   begin
      if (Get_Response (Dialog) = Default_Dialog.Response_Okay
		  and then Validate (Dialog)) then
		 
		 Operation := Get_Operation (Dialog);
		 if (Operation = Set_Operation_Dialog.Difference) then
			Controller.Subgraph_Difference (Get_Left_Source (Dialog),
											Get_Right_Source (Dialog),
											Get_Target (Dialog));
		 elsif (Operation = Set_Operation_Dialog.Intersection) then
			Controller.Subgraph_Intersection (Get_Left_Source (Dialog),
											  Get_Right_Source (Dialog),
											  Get_Target (Dialog));
		 elsif (Operation = Set_Operation_Dialog.Union) then
			Controller.Subgraph_Union (Get_Left_Source (Dialog),
									   Get_Right_Source (Dialog),
									   Get_Target (Dialog));
		 end if;
	  end if;
	  
	  return True;
   exception
	 when Projects.Subgraph_Is_Not_Part_Of_Project_Exception =>
		Controller.Show_Error (-"Please select valid sources.");
		return False;
	 when Projects.Subgraph_Is_Already_Part_Of_Project_Exception =>
		Controller.Show_Error
		  (-"The target name is already used. Please try a different name.");
		return False;
   end Can_Hide;

   ---------------------------------------------------------------------------
   --  Initializers
   ---------------------------------------------------------------------------

   procedure Create
     (Dialog : out Subgraph_Operation_Dialog_Access)
   is
   begin
      Dialog := new Subgraph_Operation_Dialog_Record;
      Initialize (Dialog);
   end Create;

   function Get_Subgraphs
     return Gtk.Enums.String_List.Glist
   is
      Source : String_Lists.List;
      Target : Gtk.Enums.String_List.Glist;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Source := Projects.Get_All_Subgraphs (Controller.Get_Project);
      Iterator := String_Lists.MakeListIter (Source);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         Gtk.Enums.String_List.Append
		   (Target, Ada.Strings.Unbounded.To_String (Name));
      end loop;
      String_Lists.Destroy (Source);

      return Target;
   end;

   procedure Initialize
     (Dialog  : access Subgraph_Operation_Dialog_Record'Class)
   is
   begin
	  Set_Operation_Dialog.Initialize (Dialog, Get_Subgraphs);
   end;

end Giant.Subgraph_Operation_Dialog;

