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
-- $RCSfile: giant-gsl-types.adb,v $, $Revision: 1.2 $
-- $Author: schulzgt $
-- $Date: 2003/06/10 11:57:25 $
--
with Unchecked_Deallocation;

package body Giant.Gsl.Types is

   --procedure Free is new Unchecked_Deallocation
   --  (Gsl_Boolean_Record,
   --   Gsl_Boolean);


   ---------------------------------------------------------------------------
   -- creates a new Gsl_Boolean var initialized with 'Value' 
   function Create_Gsl_Boolean (Value : Boolean) return Gsl_Boolean is

      Var : Gsl_Boolean;
   begin
      Var := new Gsl_Boolean_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Boolean;


   ---------------------------------------------------------------------------
   -- creates a new Gsl_Natural var 
   function Create_Gsl_Natural return Gsl_Natural is

      Var : Gsl_Natural;
   begin
      Var := new Gsl_Natural_Record;
      Var.Value := 0;
      return Var;
   end;

   ---------------------------------------------------------------------------
   -- creates a new Gsl_String var initialized with 'Value'
   function Create_Gsl_String
     (Value : String)
      return Gsl_String is

      Var : Gsl_String;
   begin
      Var := new Gsl_String_Record (Value'Length);
      Var.Value := Value;
      return Var;
   end;

   ---------------------------------------------------------------------------
   -- creates a new Gsl_Var_Reference
   function Create_Gsl_Var_Reference
     (Ref_Type : Reference_Type;
      Ref_Name : String)
      return Gsl_Var_Reference is

      Var : Gsl_Var_Reference;
   begin
      Var := new Gsl_Var_Reference_Record (Ref_Name'Length);
      Var.Ref_Type := Ref_Type;
      Var.Ref_Name := Ref_Name;
      return Var;
   end;

   ---------------------------------------------------------------------------
   -- creates a new Gsl_Script_Decl
   function Create_Gsl_Script_Reference
     (Parameter_List : Syntax_Node;
      Script_Node    : Syntax_Node)
      return Gsl_Script_Reference is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.Parameter_List := Parameter_List;
      Var.Script_Node := Script_Node;
      Var.Parent_Activation_Record := null;
      return Var;
   end;

 
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Node_Id
   function Get_Value
     (Var : Gsl_Node_Id)
      return Node_Id is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Node_Id;
      Value : Node_Id) is
   begin
      null;
   end Set_Value;

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Edge_Id
   function Get_Value
     (Var : Gsl_Edge_Id)
      return Edge_Id is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Edge_Id;
      Value : Edge_Id) is
   begin
      null;
   end Set_Value;
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Node_Set
   function Get_Value
     (Var : Gsl_Node_Set)
      return Node_Set is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Node_Set;
      Value : Node_Set) is
   begin
      null;
   end Set_Value;

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Edge_Set
   function Get_Value
     (Var : Gsl_Edge_Set)
      return Edge_Set is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Edge_Set;
      Value : Edge_Set)is
   begin
      null;
   end Set_Value;
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_String
   function Get_Value
     (Var : Gsl_String)
      return String is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_String;
      Value : String) is
   begin
      Var.Value := Value; 
   end Set_Value;

   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Boolean
   function Get_Value
     (Var : Gsl_Boolean)
      return Boolean is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Boolean;
      Value : Boolean) is
   begin
      Var.Value := Value;
   end Set_Value;
      
   ---------------------------------------------------------------------------
   -- get and set values for Gsl_Natural
   function Get_Value
     (Var : Gsl_Natural)
      return Natural is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Natural;
      Value : Natural) is
   begin
      Var.Value := Value;
   end Set_Value;

   ---------------------------------------------------------------------------
   -- access routines for Gsl_List
   function Get_List_Size
     (Var      : Gsl_List)
      return Natural is
   begin
      return Var.List_Size;
   end Get_List_Size;
	   
   function Get_Value_At
     (Var      : Gsl_List;
      Position : Natural)
      return Gsl_Type is
   begin
      return Var.Value (Position);
   end Get_Value_At;

   procedure Set_Value_At
     (Var      : Gsl_List;
      Position : Natural;
      Value    : Gsl_Type) is
   begin
      null;
   end Set_Value_At;
   
   ---------------------------------------------------------------------------
   -- access routines for Gsl_Var_Reference
   function Get_Ref_Name
     (Var : Gsl_Var_Reference)
      return String is
   begin
      return Var.Ref_Name;
   end; 

   function Get_Ref_Type
     (Var : Gsl_Var_Reference)
      return Reference_Type is
   begin
      return Var.Ref_Type;
   end;
	
end Giant.Gsl.Types;
