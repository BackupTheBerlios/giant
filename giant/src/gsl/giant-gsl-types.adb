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
-- $RCSfile: giant-gsl-types.adb,v $, $Revision: 1.4 $
-- $Author: schulzgt $
-- $Date: 2003/06/16 15:02:43 $
--
with Ada.Unchecked_Deallocation;

package body Giant.Gsl.Types is

   ---------------------------------------------------------------------------
   -- Gsl_Node_Id
   function Create_Gsl_Node_Id return Gsl_Node_Id is

      Var : Gsl_Node_Id;
   begin
      Var := new Gsl_Node_Id_Record;
      return Var;
   end Create_Gsl_Node_Id;

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

   function Copy
     (Object : access Gsl_Node_Id_Record)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Copy;

   --procedure Free is new Ada.Unchecked_Deallocation
   --  (Gsl_Node_Id_Record, Gsl_Node_Id);

   procedure Destroy
     (Object : out Gsl_Node_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Node_Id_Record, Gsl_Node_Id);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Edge_Id
   function Create_Gsl_Edge_Id return Gsl_Edge_Id is

      Var : Gsl_Edge_Id;
   begin
      Var := new Gsl_Edge_Id_Record;
      return Var;
   end Create_Gsl_Edge_Id;

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

   function Copy
     (Object : access Gsl_Edge_Id_Record)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Copy;

   procedure Destroy
     (Object : out Gsl_Edge_Id) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Edge_Id_Record, Gsl_Edge_Id);

   begin
      Free (Object);      
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Node_Set
   function Create_Gsl_Node_Set return Gsl_Node_Set is

      Var : Gsl_Node_Set;
   begin
      Var := new Gsl_Node_Set_Record;
      return Var;
   end Create_Gsl_Node_Set;

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

   function Copy
     (Object : access Gsl_Node_Set_Record)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Copy;

   procedure Destroy
     (Object : out Gsl_Node_Set) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Node_Set_Record, Gsl_Node_Set);

   begin
      Free (Object);
   end Destroy;
 
   ---------------------------------------------------------------------------
   -- Gsl_Edge_Set
   function Create_Gsl_Edge_Set return Gsl_Edge_Set is

      Var : Gsl_Edge_Set;
   begin
      Var := new Gsl_Edge_Set_Record;
      return Var;
   end Create_Gsl_Edge_Set;

   function Get_Value
     (Var : Gsl_Edge_Set)
      return Edge_Set is
   begin
      return Var.Value;
   end Get_Value;

   procedure Set_Value
     (Var   : Gsl_Edge_Set;
      Value : Edge_Set) is
   begin
      null;
   end Set_Value;

   function Copy
     (Object : access Gsl_Edge_Set_Record)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Copy;

   procedure Destroy
     (Object : out Gsl_Edge_Set) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Edge_Set_Record, Gsl_Edge_Set);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_String
   function Create_Gsl_String
     (Value : String)
      return Gsl_String is

      Var : Gsl_String;
   begin
      Var := new Gsl_String_Record (Value'Length);
      Var.Value := Value;
      return Var;
   end;

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

   function Copy
     (Object : access Gsl_String_Record)
      return Gsl_Type is

      Var : Gsl_String;
   begin
      Var := new Gsl_String_Record (Object.Value'Length);
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   procedure Destroy
     (Object : out Gsl_String) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_String_Record, Gsl_String);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Boolean
   function Create_Gsl_Boolean (Value : Boolean) return Gsl_Boolean is

      Var : Gsl_Boolean;
   begin
      Var := new Gsl_Boolean_Record;
      Var.Value := Value;
      return Var;
   end Create_Gsl_Boolean;

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

   function Copy
     (Object : access Gsl_Boolean_Record)
      return Gsl_Type is

      Var : Gsl_Boolean;
   begin
      Var := new Gsl_Boolean_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end;
      
   procedure Destroy
     (Object : out Gsl_Boolean) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Boolean_Record, Gsl_Boolean);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Natural
   function Create_Gsl_Natural return Gsl_Natural is

      Var : Gsl_Natural;
   begin
      Var := new Gsl_Natural_Record;
      Var.Value := 0;
      return Var;
   end;
  
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

   function Copy
     (Object : access Gsl_Natural_Record)
      return Gsl_Type is

      Var : Gsl_Natural;
   begin
      Var := new Gsl_Natural_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   procedure Destroy
     (Object : out Gsl_Natural) is
   
      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Natural_Record, Gsl_Natural);

    begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_List
   function Create_Gsl_List
     (Size : Natural)
      return Gsl_List is

      Var : Gsl_List;
   begin
      Var := new Gsl_List_Record (Size);
      Var.List_Size := Size;
      return Var;
   end Create_Gsl_List;

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

   function Copy
     (Object : access Gsl_List_Record)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Copy;

   procedure Destroy
     (Object : out Gsl_List) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_List_Record, Gsl_List);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Var_Reference
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

   function Get_Ref_Type
     (Var : Gsl_Var_Reference)
      return Reference_Type is
   begin
      return Var.Ref_Type;
   end;

   function Get_Ref_Name
     (Var : Gsl_Var_Reference)
      return String is
   begin
      return Var.Ref_Name;
   end; 

   function Copy
     (Object : access Gsl_Var_Reference_Record)
      return Gsl_Type is

      Var        : Gsl_Var_Reference;
   begin
      Var := new Gsl_Var_Reference_Record (Object.Ref_Name'Length);
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;

   procedure Destroy
     (Object : out Gsl_Var_Reference) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Var_Reference_Record, Gsl_Var_Reference);

   begin
      Free (Object);
   end Destroy;

   ---------------------------------------------------------------------------
   -- Gsl_Script_Decl
   function Create_Gsl_Script_Reference
     (Parameter_List : Syntax_Node;
      Script_Node    : Syntax_Node)
      return Gsl_Script_Reference is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.Script_Type := Gsl_Script;
      Var.Parameter_List := Parameter_List;
      Var.Script_Node := Script_Node;
      Var.Parent_Activation_Record := null;
      return Var;
   end;

   function Create_Gsl_Script_Reference
     (Runtime : Runtime_Function)
      return Gsl_Script_Reference is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.Script_Type := Gsl_Runtime;
      Var.Runtime := Runtime;
      return Var;
   end;

   function Get_Script_Type
     (Object : Gsl_Script_Reference)
      return Gsl_Script_Type is
   begin
      return Object.Script_Type;
   end Get_Script_Type;

   function Get_Gsl_Runtime
     (Object : Gsl_Script_Reference)
      return Runtime_Function is
   begin
      return Object.Runtime;
   end Get_Gsl_Runtime;

   function Copy
     (Object : access Gsl_Script_Reference_Record)
      return Gsl_Type is

      Var : Gsl_Script_Reference;
   begin
      Var := new Gsl_Script_Reference_Record;
      Var.all := Object.all;
      return Gsl_Type (Var);
   end Copy;
   
   procedure Destroy
     (Object : out Gsl_Script_Reference) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Gsl_Script_Reference_Record, Gsl_Script_Reference);

   begin
      Free (Object);
   end Destroy;

end Giant.Gsl.Types;
