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
-- $RCSfile: giant-gsl-runtime.adb,v $
-- $Author: schulzgt $
-- $Date: 2003/06/25 16:09:27 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Tags;
use  Ada.Tags;
with Ada.Exceptions;

with Giant.Graph_Lib;

package body Giant.Gsl.Runtime is

------------------------------------------------------------------------------
-- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)

   ---------------------------------------------------------------------------
   --
   function Runtime_Add
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param1 : Gsl_Type;
      Param2 : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 2 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'add' requires " &
             "2 parameters.");
      end if;
      Param1 := Get_Value_At (Parameter, 1);
      Param2 := Get_Value_At (Parameter, 2);
      if (Param1 = Gsl_Null) or (Param2 = Gsl_Null) then
         null;
      elsif (Param1'Tag = Gsl_Natural_Record'Tag) and
            (Param2'Tag = Gsl_Natural_Record'Tag) then
         null;
      elsif Param1'Tag = Gsl_Var_Reference_Record'Tag then
         null;
      else
         null;
      end if;

      return Gsl_Null;
   end Runtime_Add;

   ---------------------------------------------------------------------------
   --
   function Runtime_Sub
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Sub;

   ---------------------------------------------------------------------------
   --
   function Runtime_Cat
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Cat;

------------------------------------------------------------------------------
-- compare (ref. GIANT Scripting Language Specification 1.5.1.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Less
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Less;

   ---------------------------------------------------------------------------
   --
   function Runtime_Equal
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Equal;

   ---------------------------------------------------------------------------
   --
   function Runtime_In_Regexp
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_In_Regexp;

   ---------------------------------------------------------------------------
   --
   function Runtime_Type_In
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Type_In;

------------------------------------------------------------------------------
-- sets and lists (ref. GIANT Scripting Language Specification 1.5.1.5)

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Empty_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Empty_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_For_Each
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_For_Each;

   ---------------------------------------------------------------------------
   --
   function Runtime_Size_Of
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Size_Of;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Entry
     (Parameter : Gsl_List)
      return Gsl_Type is
   begin
      return Gsl_Null;
   end Runtime_Get_Entry;

------------------------------------------------------------------------------
-- types (ref. GIANT Scripting Language Specification 1.5.1.6)

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Nodeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Node_Id_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Nodeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edgeid
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edgeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Edge_Id_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Edgeid;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_node_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Node_Set_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Node_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_edge_set' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Edge_Set_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Edge_Set;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_String
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_string' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_String_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_String;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Boolean
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_boolean' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Boolean_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Boolean;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Natural
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_natural' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Natural_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Natural;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_List
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_list' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_List_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_List;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Reference
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_reference' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Var_Reference_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Reference;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Script
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_script' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then
         return Gsl_Type (Create_Gsl_Boolean (false));
      elsif Param'Tag = Gsl_Script_Reference_Record'Tag then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Script;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Null
     (Parameter : Gsl_List)
      return Gsl_Type is

      Param : Gsl_Type;
   begin
      if Get_List_Size (Parameter) /= 1 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'is_nodeid' requires " &
             "1 parameter.");
      end if;
      Param := Get_Value_At (Parameter, 1);
      if Param = Gsl_Null then 
         return Gsl_Type (Create_Gsl_Boolean (true));
      else
         return Gsl_Type (Create_Gsl_Boolean (false));
      end if;
   end Runtime_Is_Null;

------------------------------------------------------------------------------
-- IML graph (ref. GIANT Scripting Language Specification 1.5.1.7)

   ---------------------------------------------------------------------------
   --
   function Runtime_Root_Node
     (Parameter : Gsl_List)
      return Gsl_Type is

      Node : Gsl_Node_Id;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'root_node' requires " &
             "no parameters.");
      end if;
      Node := Create_Gsl_Node_Id;
      Set_Value (Node, Graph_Lib.Get_Root_Node);
      return Gsl_Type (Node);
   end Runtime_Root_Node;

   ---------------------------------------------------------------------------
   --
   function Runtime_All_Nodes
     (Parameter : Gsl_List)
      return Gsl_Type is

      Nodes : Gsl_Node_Set;
   begin
      if Get_List_Size (Parameter) /= 0 then
         Ada.Exceptions.Raise_Exception
           (Gsl_Runtime_Error'Identity, "Script 'all_nodes' requires " &
             "no parameters.");
      end if;
      Nodes := Create_Gsl_Node_Set;
      Set_Value (Nodes, Graph_Lib.Get_All_Nodes);
      return Gsl_Type (Nodes);
   end Runtime_All_Nodes;

end Giant.Gsl.Runtime;
