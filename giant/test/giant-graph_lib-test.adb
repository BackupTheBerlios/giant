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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-graph_lib-test.adb,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/06/15 12:45:43 $
--

with Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;

with Giant.Graph_Lib; use Giant.Graph_Lib;
with Giant.Default_Logger;
with Giant.Logger;

package body Giant.Graph_Lib.Test is

   package Logger is new Giant.Logger("giant.graph_lib");

   procedure Test_Init (R : in out AUnit.Test_Cases.Test_Case'Class)
   is
	  procedure Output_Attributes (Node : in Node_Id) is
		 Iter : Node_Attribute_Iterator;
		 Attr : Node_Attribute_Id;
	  begin
		 Iter := Make_Attribute_Iterator (Node);
		 while More (Iter) loop
			Next (Iter, Attr);
			
			Logger.Debug ("Attribute " & Convert_Node_Attribute_Id_To_Name (Attr));
			
			case Get_Node_Attribute_Class_Id (Attr) is
			   when Class_Node_Id =>
				  null;
			   when Class_SLoc =>
				  null;
			   when others =>
				  null;
			end case;
			
		 end loop;
	  end Output_Attributes;

      Root : Node_Id;
   begin
	  Giant.Graph_Lib.Create ("resources/rfg_examp.iml");

      Root := Get_Root_Node;
      Logger.Debug ("Got Root Node with ID: " & Node_Id_Image (Root));
	  Output_Attributes (Root);

	  --Assert (Ada.Text_IO.Is_Open (Out_File), "Is_Open");

	  Giant.Graph_Lib.Destroy;
   end;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access is
   begin
      return new String'("Default_Logger");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Init'Access, "Init");
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
	  Default_Logger.Init;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
	  Default_Logger.Close;
   end Tear_Down;

end Giant.Graph_Lib.Test;
