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
--  $RCSfile: giant-graph_lib-selections.adb,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/06/14 08:32:37 $

package body Giant.Graph_Lib.Selections is

   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Selection;
      Right : in Selection)
      return Boolean
   is
   begin
      return False;
   end "<";

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge
     (Selection_To_Modify : in out Selection;
      Edge                : in     Edge_Id)
   is
   begin
      null;
   end Add_Edge;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (Selection_To_Modify : in out Selection;
      Node                : in     Node_Id)
   is
   begin
      null;
   end Add_Node;

   ------------------
   -- Add_Node_Set --
   ------------------

   procedure Add_Node_Set
     (Selection_To_Modify : in out Selection;
      Node_Set            : in     Node_Id_Set)
   is
   begin
      null;
   end Add_Node_Set;

   ------------------
   -- Add_Node_Set --
   ------------------

   procedure Add_Node_Set
     (Selection_To_Modify : in out Selection;
      Edge_Set            : in     Edge_Id_Set)
   is
   begin
      null;
   end Add_Node_Set;

   -----------
   -- Clone --
   -----------

   function Clone
     (Selection_To_Clone : in Selection)
      return Selection
   is
   begin
      --  TBD
      return new Selection_Record (Name_Length => 1);
   end Clone;

   ---------------------------------------------------------------------------
   --  Creates a new Selection with no nodes and no edges
   --
   function Create
     (Name : in    Valid_Names.Standard_Name)
      return Selection
   is
   begin
      --  TBD
      return new Selection_Record (Name_Length => 1);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Selection_To_Destroy : in out Selection)
   is
   begin
      null;
   end Destroy;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Selection_To_Read : in Selection)
      return String
   is
   begin
      --  TBD
      return "";
   end Get_Name;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
     (Left  : in Selection;
      Right : in Selection)
      return Selection
   is
   begin
      --  TBD
      return new Selection_Record (Name_Length => 1);
   end Intersection;

   -----------------
   -- Remove_Edge --
   -----------------

   procedure Remove_Edge
     (Selection_To_Modify : in out Selection;
      Edge                : in     Edge_Id)
   is
   begin
      null;
   end Remove_Edge;

   ---------------------
   -- Remove_Edge_Set --
   ---------------------

   procedure Remove_Edge_Set
     (Selection_To_Modify : in out Selection;
      Edge_Set            : in     Edge_Id_Set)
   is
   begin
      null;
   end Remove_Edge_Set;

   ----------------------------------------------------------------------------
   procedure Remove_Node
     (Selection_To_Modify : in out Selection;
      Node                : in     Node_Id)
   is
   begin
      begin
         Node_Id_Sets.Remove (Selection_To_Modify.Nodes,
                              Node);
      exception
         when Node_Id_Sets.No_Member =>
            raise Node_Does_Not_Exist;
      end;
   end Remove_Node;

   ----------------------------------------------------------------------------
   procedure Remove_Node_Set
     (Selection_To_Modify : in out Selection;
      Node_Set            : in     Node_Id_Set)
   is
      procedure Execute (Node : in Node_Id) is
      begin
         Remove_Node (Selection_To_Modify,
                      Node);
      end Execute;

      procedure Apply is new Node_Id_Sets.Apply (Execute => Execute);

   begin
      Apply (Node_Set);
   end Remove_Node_Set;

   ----------------------------------------------------------------------------
   procedure Rename
     (Selection_To_Rename : in out Selection;
      New_Name            : in     Valid_Names.Standard_Name)
   is
      Res  : Selection;
      Name : String := Valid_Names.To_String (New_Name);
   begin
      Res       := new Selection_Record (Name_Length => Name'Length);
      Res.Name  := Name;
      Res.Edges := Selection_To_Rename.Edges;
      Res.Nodes := Selection_To_Rename.Nodes;

      Destroy (Selection_To_Rename);
      Selection_To_Rename := Res;
   end Rename;

   ----------------------------------------------------------------------------
   procedure Selection_Read
     (Stream            : in      Bauhaus_Io.In_Stream_Type;
      Selection_To_Read :     out Selection)
   is
   begin
      --  TBD
      Selection_To_Read := new Selection_Record (Name_Length => 1);
   end Selection_Read;

   ----------------------------------------------------------------------------
   procedure Selection_Write
     (Stream             : in Bauhaus_Io.Out_Stream_Type;
      Selection_To_Write : in Selection)
   is
   begin
      --  TBD
      null;
   end Selection_Write;

   -------------------------
   -- Symetric_Difference --
   -------------------------

   function Symetric_Difference
     (Left  : in Selection;
      Right : in Selection)
      return Selection
   is
   begin
      --  TBD
      return new Selection_Record (Name_Length => 1);
   end Symetric_Difference;

   ----------------------------------------------------------------------------
   function Union
     (Left  : in Selection;
      Right : in Selection)
      return Selection
   is
      Res : Selection;
   begin
      --  TBD
      Res := new Selection_Record (Name_Length => 1);
      return Res;
   end Union;

end Giant.Graph_Lib.Selections;
