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
-- $RCSfile: giant-gsl_types.adb,v $, $Revision: 1.1 $
-- $Author: schulzgt $
-- $Date: 2003/05/27 13:43:18 $
--
with Unchecked_Deallocation;

package body Giant.Gsl_Types is

   procedure Free is new Unchecked_Deallocation
     (Gsl_Type_Record,
      Gsl_Type);

   ---------------------------------------------------------------------------
   -- get and set the name for a Gsl_Type
   function Get_Name
     (Var : Gsl_Type)
      return Unbounded_String is
   begin      
      return Var.Name;
   end Get_Name;

   procedure Set_Name
     (Var   : Gsl_Type;
      Value : Unbounded_String) is
   begin
      null;
   end Set_Name;
   
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
      return Unbounded_String is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_String;
      Value : Unbounded_String) is
   begin
      null;
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
      null;
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
      null;
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
	
end Giant.Gsl_Types;
