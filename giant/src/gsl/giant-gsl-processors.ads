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
-- $RCSfile: giant-gsl-processors.ads,v $
-- $Author: schulzgt $
-- $Date: 2003/07/31 09:11:39 $
--
-- This package implements the Gsl interpreter.
--

with Giant.Gsl.Types;
use  Giant.Gsl.Types;

package Giant.Gsl.Processors is

   ---------------------------------------------------------------------------
   --
   procedure Execute
     (Cmd : Syntax_Node);

private

   ---------------------------------------------------------------------------
   --
   procedure Script_Activation_Cmd;

   ---------------------------------------------------------------------------
   --
   procedure Script_Exec_Cmd;

   ---------------------------------------------------------------------------
   --
   function Get_Subgraph
     (Name : String)
      return Gsl_Type;
   
   ---------------------------------------------------------------------------
   --
   function Get_Subgraph_Reference
     (Ref : Gsl_Var_Reference)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Selection
     (Name : String)
      return Gsl_Type;

   ---------------------------------------------------------------------------
   --
   function Get_Selection_Reference
     (Ref : Gsl_Var_Reference)
      return Gsl_Type;

end Giant.Gsl.Processors;
