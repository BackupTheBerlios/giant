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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-graph_lib-selections.ads,v $, $Revision: 1.4 $
--  $Author: koppor $
--  $Date: 2003/06/10 09:25:22 $
--
------------------------------------------------------------------------------
--
--  Contains the Selections

--  from Bauhaus
with Lists;
with Bauhaus_Io;

--  from GIANT
with Giant.Valid_Names;

package Giant.Graph_Lib.Selections is

   ---------------------------------------------------------------------------
   --  It is assured, that a copy makes an alias
   type Selection is private;

   ---------------------------------------------------------------------------
   --  Creates a new selection,
   --    which has to be destroyed afterwards
   function Create
      (Name : in    Valid_Names.Standard_Name)
      return Selection;

   ---------------------------------------------------------------------------
   --  Removes given selection from memory
   procedure Destroy
      (Selection_To_Destroy : in out Selection);

   ---------------------------------------------------------------------------
   --  Creates a deep-copy of the selection
   function Clone
      (Selection_To_Clone : in Selection)
      return Selection;

   ---------------------------------------------------------------------------
   procedure Rename
      (Selection_To_Rename : in out Selection;
       New_Name            : in     Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   function Get_Name
      (Selection_To_Read : in Selection)
      return String;

   ---------------------------------------------------------------------------
   function "<"
      (Left  : in Selection;
       Right : in Selection)
      return Boolean;

   ---------------
   --  Streams  --
   ---------------

   ---------------------------------------------------------------------------
   --  cp. Barnes p543
   --  Maybe u have to use "for Selection'Write use Selection_Write"
   procedure Selection_Write
      (Stream             : in Bauhaus_Io.Out_Stream_Type;
       Selection_To_Write : in Selection);

   ---------------------------------------------------------------------------
   procedure Selection_Read
      (Stream            : in Bauhaus_Io.In_Stream_Type;
       Selection_To_Read : in Selection);


   -----------------------
   --  Routines to add  --
   -----------------------

   ---------------------------------------------------------------------------
   --  Adds given Node to the given Selection
   procedure Add_Node
      (Selection_To_Modify : in out Selection;
       Node                : in     Node_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Selection
   procedure Add_Node_Set
      (Selection_To_Modify : in out Selection;
       Node_Set            : in     Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Adds given Edge to the given Selection
   procedure Add_Edge
      (Selection_To_Modify : in out Selection;
       Edge                : in     Edge_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Selection
   procedure Add_Node_Set
      (Selection_To_Modify : in out Selection;
       Edge_Set            : in     Edge_Id_Set);

   --------------------------
   --  Routines to remove  --
   --------------------------

   ---------------------------------------------------------------------------
   --  Adds given Node to the given Selection
   procedure Remove_Node
      (Selection_To_Modify : in out Selection;
       Node                : in     Node_Id);

   ---------------------------------------------------------------------------
   --  Adds all nodes in given set to the Selection
   procedure Remove_Node_Set
      (Selection_To_Modify : in out Selection;
       Node_Set            : in     Node_Id_Set);

   ---------------------------------------------------------------------------
   --  Adds given Edge to the given Selection
   procedure Remove_Edge
      (Selection_To_Modify : in out Selection;
       Edge                : in     Edge_Id);

   ---------------------------------------------------------------------------
   --  Adds all edges in given set to the Selection
   procedure Remove_Edge_Set
      (Selection_To_Modify : in out Selection;
       Edge_Set            : in     Edge_Id_Set);

   ----------------------
   --  Set-operations  --
   ----------------------

   ---------------------------------------------------------------------------
   --  Creates a new selection where the two given Selections are unified
   function Union
      (Left  : in Selection;
       Right : in Selection)
      return Selection;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Symetric_Difference
      (Left  : in Selection;
       Right : in Selection)
      return Selection;

   ---------------------------------------------------------------------------
   --  Creates a new selection
   function Intersection
      (Left  : in Selection;
       Right : in Selection)
      return Selection;

private
   type Selection_Record is null record;

   type Selection is access Selection_Record;

end Giant.Graph_Lib.Selections;

