------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Gerrit Schulz
--
-- $RCSfile: giant-gsl-types.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/06/10 11:57:25 $
--
-- This package implements the datatypes used in GSL.
--

--with Graph_Lib;
--use  Graph_Lib;

package Giant.Gsl.Types is

   ---------------------------------------------------------------------------
   -- only temporary
   type Node_Id is new Integer;
   type Edge_Id is new Integer;
   type Node_Set is new Integer;
   type Edge_Set is new Integer;

   ---------------------------------------------------------------------------
   -- gsl types (ref. GIANT Scripting Language Specification: 1.3.1)

   type Gsl_Node_Id_Record is new Gsl_Type_Record with private;
   type Gsl_Node_Id is access all Gsl_Node_Id_Record'Class;

   type Gsl_Edge_Id_Record is new Gsl_Type_Record with private;
   type Gsl_Edge_Id is access all Gsl_Edge_Id_Record'Class;

   type Gsl_Node_Set_Record is new Gsl_Type_Record with private;
   type Gsl_Node_Set is access all Gsl_Node_Set_Record'Class;

   type Gsl_Edge_Set_Record is new Gsl_Type_Record with private;
   type Gsl_Edge_Set is access all Gsl_Edge_Set_Record'Class;

   type Gsl_String_Record (Size : Natural) is new Gsl_Type_Record with private;
   type Gsl_String is access all Gsl_String_Record'Class;

   type Gsl_Boolean_Record is new Gsl_Type_Record with private;
   type Gsl_Boolean is access all Gsl_Boolean_Record'Class;

   type Gsl_Natural_Record is new Gsl_Type_Record with private;
   type Gsl_Natural is access all Gsl_Natural_Record'Class;

   ---------------------------------------------------------------------------
   -- 
   type Gsl_List_Record (Size : Natural) is new Gsl_Type_Record with private;
   type Gsl_List is access all Gsl_List_Record'Class;

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference ("local" Var or "global" Subgraph, Selection)
   type Reference_Type is (Var, Subgraph, Selection);
   type Gsl_Var_Reference_Record (Size : Natural) is new 
     Gsl_Type_Record with private;
   type Gsl_Var_Reference is access all Gsl_Var_Reference_Record'Class;

   type Gsl_Script_Reference_Record is new Gsl_Type_Record with private;
   type Gsl_Script_Reference is access all Gsl_Script_Reference_Record'Class;

   --Gsl_Null : constant Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Boolean
     (Value : Boolean)
      return Gsl_Boolean;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Natural return Gsl_Natural;
   
   ---------------------------------------------------------------------------
   --
   function Create_Gsl_String
     (Value : String) 
      return Gsl_String;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Var_Reference
     (Ref_Type : Reference_Type;
      Ref_Name : String) 
      return Gsl_Var_Reference;

   ---------------------------------------------------------------------------
   --
   function Create_Gsl_Script_Reference
     (Parameter_List : Syntax_Node;
      Script_Node    : Syntax_Node) 
      return Gsl_Script_Reference;

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Node_Id
   function Get_Value
     (Var : Gsl_Node_Id)
      return Node_Id;

   procedure Set_Value
     (Var   : Gsl_Node_Id;
      Value : Node_Id);

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Edge_Id
   function Get_Value
     (Var : Gsl_Edge_Id)
      return Edge_Id;

   procedure Set_Value
     (Var   : Gsl_Edge_Id;
      Value : Edge_Id);
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Node_Set
   function Get_Value
     (Var : Gsl_Node_Set)
      return Node_Set;

   procedure Set_Value
     (Var   : Gsl_Node_Set;
      Value : Node_Set);

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Edge_Set
   function Get_Value
     (Var : Gsl_Edge_Set)
      return Edge_Set;

   procedure Set_Value
     (Var   : Gsl_Edge_Set;
      Value : Edge_Set);
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_String
   function Get_Value
     (Var : Gsl_String)
      return String;

   procedure Set_Value
     (Var   : Gsl_String;
      Value : String);

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Boolean
   function Get_Value
     (Var : Gsl_Boolean)
      return Boolean;

   procedure Set_Value
     (Var   : Gsl_Boolean;
      Value : Boolean);
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Natural
   function Get_Value
     (Var : Gsl_Natural)
      return Natural;

   procedure Set_Value
     (Var   : Gsl_Natural;
      Value : Natural);

   ---------------------------------------------------------------------------
   -- access routines for Gsl_List
   function Get_List_Size
     (Var      : Gsl_List)
      return Natural;
	   
   function Get_Value_At
     (Var      : Gsl_List;
      Position : Natural)
      return Gsl_Type;

   procedure Set_Value_At
     (Var      : Gsl_List;
      Position : Natural;
      Value    : Gsl_Type);
  
   ---------------------------------------------------------------------------
   -- access routines for Gsl_Var_Reference
   function Get_Ref_Name
     (Var : Gsl_Var_Reference)
      return String;

   function Get_Ref_Type 
     (Var : Gsl_Var_Reference)
      return Reference_Type;

------------------------------------------------------------------------------
-- private part 
private

   ---------------------------------------------------------------------------
   -- Gsl_Node_Id
   type Gsl_Node_Id_Record is new Gsl_Type_Record with
      record
         Value : Node_Id;
      end record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Edge_Id
   type Gsl_Edge_Id_Record is new Gsl_Type_Record with
      record
         Value : Edge_Id;
      end record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Node_Set
   type Gsl_Node_Set_Record is new Gsl_Type_Record with
      record
         Value : Node_Set;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Set
   type Gsl_Edge_Set_Record is new Gsl_Type_Record with
      record
         Value : Edge_Set;
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_String
   type Gsl_String_Record (Size : Natural) is new Gsl_Type_Record with
      record
         Value : String (1 .. Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Boolean
   type Gsl_Boolean_Record is new Gsl_Type_Record with
      record
         Value : Boolean;
      end record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Natural
   type Gsl_Natural_Record is new Gsl_Type_Record with
      record
         Value : Natural;
      end record;
   
   ---------------------------------------------------------------------------
   -- Gsl_List
   type Gsl_List_Record (Size : Natural) is new Gsl_Type_Record with
      record
         List_Size : Natural;
         Value     : Gsl_Type_Array (1 .. Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference
   type Gsl_Var_Reference_Record (Size : Natural) is new Gsl_Type_Record with
      record
         Ref_Type : Reference_Type;
         Ref_Name : String (1 .. Size);
      end record;

   ---------------------------------------------------------------------------
   -- Gsl_Script_Reference
   type Gsl_Script_Reference_Record is new Gsl_Type_Record with
      record
         Parameter_List           : Syntax_Node;
         Script_Node              : Syntax_Node;
         Parent_Activation_Record : Activation_Record;
      end record;

end Giant.Gsl.Types;
