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
-- $RCSfile: giant-gsl_types.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/05/27 13:43:18 $
--
-- This package implements the datatypes used in GSL.
--
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

--with Graph_Lib;
--use  Graph_Lib;

package Giant.Gsl_Types is

   ---------------------------------------------------------------------------
   -- only temporary
   type Node_Id is new Integer;
   type Edge_Id is new Integer;
   type Node_Set is new Integer;
   type Edge_Set is new Integer;
	
   ---------------------------------------------------------------------------
   -- gsl types (ref. GIANT Scripting Language Specification: 1.3.1)
   type Gsl_Type is private;
   type Gsl_Node_Id is private;
   type Gsl_Edge_Id is private;
   type Gsl_Node_Set is private;
   type Gsl_Edge_Set is private;
   type Gsl_String is private;
   type Gsl_Boolean is private;
   type Gsl_Natural is private;
   type Gsl_List is private;
   type Gsl_Var_Reference is private;
   type Gsl_Script_Reference is private;
   type Gsl_Null is private;

   ---------------------------------------------------------------------------
   -- get and set the name for a Gsl_Type
   function Get_Name
     (Var : Gsl_Type)
      return Unbounded_String;

   procedure Set_Name
     (Var   : Gsl_Type;
      Value : Unbounded_String);
   
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
      return Unbounded_String;

   procedure Set_Value
     (Var   : Gsl_String;
      Value : Unbounded_String);

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
   
------------------------------------------------------------------------------
-- private part 
private

   ---------------------------------------------------------------------------
   -- Gsl_Type - parent class for all types in GSL
   type Gsl_Type_Record is tagged
      record
         Name : Unbounded_String;
      end record;

   type Gsl_Type is access Gsl_Type_Record;

   type Gsl_Type_Array is array (Positive range <>) of Gsl_Type;

   ---------------------------------------------------------------------------
   -- Gsl_Node_Id
   type Gsl_Node_Id_Record is new Gsl_Type_Record with
      record
         Value : Node_Id;
      end record;

   type Gsl_Node_Id is access Gsl_Node_Id_Record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Edge_Id
   type Gsl_Edge_Id_Record is new Gsl_Type_Record with
      record
         Value : Edge_Id;
      end record;

   type Gsl_Edge_Id is access Gsl_Edge_Id_Record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Node_Set
   type Gsl_Node_Set_Record is new Gsl_Type_Record with
      record
         Value : Node_Set;
      end record;

   type Gsl_Node_Set is access Gsl_Node_Set_Record;

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Set
   type Gsl_Edge_Set_Record is new Gsl_Type_Record with
      record
         Value : Edge_Set;
      end record;

   type Gsl_Edge_Set is access Gsl_Edge_Set_Record;

   ---------------------------------------------------------------------------
   -- Gsl_String
   type Gsl_String_Record is new Gsl_Type_Record with
      record
         Value : Unbounded_String;
      end record;

   type Gsl_String is access Gsl_String_Record;

   ---------------------------------------------------------------------------
   -- Gsl_Boolean
   type Gsl_Boolean_Record is new Gsl_Type_Record with
      record
         Value : Boolean;
      end record;

   type Gsl_Boolean is access Gsl_Boolean_Record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Natural
   type Gsl_Natural_Record is new Gsl_Type_Record with
      record
         Value : Natural;
      end record;

   type Gsl_Natural is access Gsl_Natural_Record;
   
   ---------------------------------------------------------------------------
   -- Gsl_List
   type Gsl_List_Record (Size : Natural) is new Gsl_Type_Record with
      record
         List_Size : Natural;
         Value     : Gsl_Type_Array (1 .. Size);
      end record;

   type Gsl_List is access Gsl_List_Record;

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference
   type Gsl_Var_Reference_Record is new Gsl_Type_Record with
      record
         val : Natural;
      end record;

   type Gsl_Var_Reference is access Gsl_Var_Reference_Record;

   ---------------------------------------------------------------------------
   -- Gsl_Script_Reference
   type Gsl_Script_Reference_Record is new Gsl_Type_Record with
      record
         val : Natural;
      end record;

   type Gsl_Script_Reference is access Gsl_Script_Reference_Record;
   
   ---------------------------------------------------------------------------
   -- Gsl_Null
   --type Gsl_Null_Record is new Gsl_Type_Record;

   type Gsl_Null is access Gsl_Type_Record;
end Giant.Gsl_Types;
