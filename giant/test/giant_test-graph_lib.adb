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
--  $RCSfile: giant_test-graph_lib.adb,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/06/13 17:59:02 $
--
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with Giant; use Giant;

with Giant.Graph_Lib; use Giant.Graph_Lib;

with Giant.Default_Logger;

procedure Giant_Test.Graph_Lib is

   procedure Output_Attributes (Node : in Node_Id) is
      Iter : Node_Attribute_Iterator;
      Attr : Node_Attribute_Id;
   begin
      Iter := Make_Attribute_Iterator (Node);
      while More (Iter) loop
         Next (Iter, Attr);

         Put_Line ("Attribute " & Convert_Node_Attribute_Id_To_Name (Attr));

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

begin
   Giant.Default_Logger.Init;
   Giant.Graph_Lib.Create ("/home/stsopra/giant/graphs/rfg_examp.iml");

   -- q&d test of getting a single node
   declare
      Root : Node_Id;
   begin
      Root := Get_Root_Node;
      Put ("Got Root Node with ID ");
      Put_Line (Node_Id_Image (Root));
   end;

   Giant.Graph_Lib.Destroy;
   Giant.Default_Logger.Close;
end Giant_Test.Graph_Lib;
