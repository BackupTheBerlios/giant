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
-- $RCSfile: giant-gsl-runtime.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/06/23 14:28:32 $
--
-- This package implements the Gsl Runtime Library 
--

with Giant.Gsl.Types;
use  Giant.Gsl.Types;

package Giant.Gsl.Runtime is

------------------------------------------------------------------------------
-- arithmetic (ref. GIANT Scripting Language Specification 1.5.1.3)

   ---------------------------------------------------------------------------
   --
   function Runtime_Add
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Sub
     (Parameter : Gsl_List)
      return Gsl_Type;

------------------------------------------------------------------------------
-- compare (ref. GIANT Scripting Language Specification 1.5.1.4)

   ---------------------------------------------------------------------------
   --
   function Runtime_Less
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Equal
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_In_Regexp
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Type_In
     (Parameter : Gsl_List)
      return Gsl_Type;

------------------------------------------------------------------------------
-- sets and lists (ref. GIANT Scripting Language Specification 1.5.1.5)

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Empty_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_For_Each
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Size_Of
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Get_Entry
     (Parameter : Gsl_List)
      return Gsl_Type;

------------------------------------------------------------------------------
-- types (ref. GIANT Scripting Language Specification 1.5.1.6)

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Nodeid
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edgeid
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Node_Set
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Edge_Set
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_String
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Boolean
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Natural
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_List
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Reference
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Script
     (Parameter : Gsl_List)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Runtime_Is_Null
     (Parameter : Gsl_List)
      return Gsl_Type;

end Giant.Gsl.Runtime;
