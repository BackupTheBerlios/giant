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
-- $RCSfile: giant-gsl.adb,v $
-- $Author: schulzgt $
-- $Date: 2003/08/29 14:27:10 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Tags;
use  Ada.Tags;

with String_Hash;

with Giant.Graph_Lib;

with Giant.Gsl.Types;
use  Giant.Gsl.Types;
with Giant.Gsl.Syntax_Tree;
use  Giant.Gsl.Syntax_Tree;

package body Giant.Gsl is

   ---------------------------------------------------------------------------
   --
   function Gsl_Var_Hash
     (K : Unbounded_String)
      return Integer is
   begin
      return String_Hash (To_String (K));
   end Gsl_Var_Hash;

   ---------------------------------------------------------------------------
   --
   function Gsl_Type_Image
     (Object : Gsl_Type)
      return String is

      use Giant.Graph_Lib.Node_Id_Sets;
      use Giant.Graph_Lib.Edge_Id_Sets;
   begin
      if Object = Gsl_Null then
         return "Gsl_Null";

      elsif Object'Tag = Gsl_Node_Id_Record'Tag then
         return "Gsl_Node_Id - " &
                Graph_Lib.Node_Id_Image (Get_Value (Gsl_Node_Id (Object)));

      elsif Object'Tag = Gsl_Edge_Id_Record'Tag then
         return "Gsl_Edge_Id";

      elsif Object'Tag = Gsl_Node_Set_Record'Tag then
         return "Gsl_Node_Set - Size: " &
                Size (Get_Value (Gsl_Node_Set (Object)))'Img;

      elsif Object'Tag = Gsl_Edge_Set_Record'Tag then
         return "Gsl_Edge_Set - Size: " &
                Size (Get_Value (Gsl_Edge_Set (Object)))'Img;

      elsif Object'Tag = Gsl_String_Record'Tag then
         return "Gsl_String - " & Get_Value (Gsl_String (Object));

      elsif Object'Tag = Gsl_Boolean_Record'Tag then
         return "Gsl_Boolean - " & Get_Value (Gsl_Boolean (Object))'Img;

      elsif Object'Tag = Gsl_Natural_Record'Tag then
         return "Gsl_Natural - " & Get_Value (Gsl_Natural (Object))'Img;

      elsif Object'Tag = Gsl_List_Record'Tag then
         return "Gsl_List";

      elsif Object'Tag = Gsl_Var_Reference_Record'Tag then
         return "Gsl_Var_Reference";

      elsif Object'Tag = Gsl_Script_Reference_Record'Tag then
         return "Gsl_Script_Reference";

      else
         return "Unknown Type";
      end if;
   end Gsl_Type_Image;

   ---------------------------------------------------------------------------
   --
   function Syntax_Node_Image
     (Node : Syntax_Node)
      return String is

   begin
      if Node /= Null_Node then
         case Get_Node_Type (Node) is
            when Literal           => return "Literal";
            when Visible_Var       => return "Visible_Var";
            when Global_Var        => return "Global_Var";
            when Visible_Ref       => return "Visible_Ref";
            when Var_Creation      => return "Var_Creation";
            when Global_Ref        => return "Global_Ref";
            when Script_Decl       => return "Script_Decl";
            when List              => return "List";
            when Sequence          => return "Sequence";
            when Script_Activation => return "Script_Activation";
            when Script_Exec       => return "Script_Exec";
            when Script_Finish     => return "Script_Finish";
            when Script_Loop       => return "Script_Loop";
            when Result_Pop        => return "Result_Pop";
            when Param_Fetch       => return "Param_Fetch";
         end case;
      end if;
      return "Null_Node";
   end Syntax_Node_Image;

end Giant.Gsl;
