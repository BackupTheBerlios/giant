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
-- $Date: 2003/07/29 13:47:47 $
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

   function Gsl_Var_Hash
     (K : Unbounded_String)
      return Integer is
   begin
      return String_Hash (To_String (K));
   end Gsl_Var_Hash;

   procedure Log_Syntax_Node
     (Node : Syntax_Node) is

      L  : Gsl_Type;
      LS : Gsl_String;
      LN : Gsl_Natural;
      LR : Gsl_Var_Reference;
   begin
      if Node /= Null_Node then
         case Get_Node_Type (Node) is
            when Literal =>
               L := Get_Literal (Node);
               if L = Gsl_Null then
                  Default_Logger.Debug
                    ("Syntax_Node LITERAL = Null", "Giant.Gsl");
               else
                  if L'Tag = Gsl_Boolean_Record'Tag then
                     Default_Logger.Debug
                       ("Syntax_Node LITERAL = Boolean", "Giant.Gsl");
                  elsif L'Tag = Gsl_String_Record'Tag then
                     Default_Logger.Debug
                       ("Syntax_Node LITERAL = String", "Giant.Gsl");
                     LS := Gsl_String (L);
                     Default_Logger.Debug
                       ("   String.Value = " & Get_Value (LS), "Giant.Gsl");
                  elsif L'Tag = Gsl_Natural_Record'Tag then
                     Default_Logger.Debug
                       ("Syntax_Node LITERAL = Natural", "Giant.Gsl");
                     LN := Gsl_Natural (L);
                     Default_Logger.Debug
                       ("   Natural.Value = " & Get_Value (LN)'Img, 
                        "Giant.Gsl");
                  end if;
               end if;

            when Visible_Var =>
               Default_Logger.Debug
                 ("Syntax_Node VISIBLE_VAR", "Giant.Gsl");
               LR := Gsl_Var_Reference (Get_Literal (Node));
               Default_Logger.Debug
                 ("   Visible_Var.Ref_Name = " & Get_Ref_Name (LR), 
                  "Giant.Gsl");

            when Global_Var =>
               Default_Logger.Debug
                 ("Syntax_Node GLOBAL_VAR", "Giant.Gsl");

            when Visible_Ref =>
               Default_Logger.Debug
                 ("Syntax_Node VISIBLE_REF", "Giant.Gsl");

            when Var_Creation =>
               Default_Logger.Debug
                 ("Syntax_Node VAR_CREATION", "Giant.Gsl");

            when Global_Ref =>
               Default_Logger.Debug
                 ("Syntax_Node GLOBAL_REF", "Giant.Gsl");

            when Script_Decl =>
               Default_Logger.Debug
                 ("Syntax_Node SCRIPT_DECL", "Giant.Gsl");

            when List =>
               Default_Logger.Debug
                 ("Syntax_Node LIST Size = " & Get_Size (Node)'Img,
                  "Giant.Gsl");

            when Sequence =>
               Default_Logger.Debug
                 ("Syntax_Node SEQUENCE Size =" & Get_Size (Node)'Img,
                  "Giant.Gsl");

            when Script_Activation =>
               Default_Logger.Debug
                 ("Syntax_Node SCRIPT_ACTIVATION", "Giant.Gsl");

            when others =>
               Default_Logger.Debug
                 ("Syntax_Node UNKNOWN", "Giant.Gsl");
         end case;
      end if;
   end Log_Syntax_Node;

   function Log_Gsl_Type
     (Var : Gsl_Type) 
      return String is

      use Giant.Graph_Lib.Node_Id_Sets;
      use Giant.Graph_Lib.Edge_Id_Sets;
   begin
      if Var = Gsl_Null then
         return "Gsl_Null";

      elsif Var'Tag = Gsl_Node_Id_Record'Tag then
         return "Gsl_Node_Id - " &
                Graph_Lib.Node_Id_Image (Get_Value (Gsl_Node_Id (Var)));

      elsif Var'Tag = Gsl_Edge_Id_Record'Tag then
         return "Gsl_Edge_Id";

      elsif Var'Tag = Gsl_Node_Set_Record'Tag then
         return "Gsl_Node_Set - Size: " &
                Size (Get_Value (Gsl_Node_Set (Var)))'Img;

      elsif Var'Tag = Gsl_Edge_Set_Record'Tag then
         return "Gsl_Edge_Set - Size: " &
                Size (Get_Value (Gsl_Edge_Set (Var)))'Img;

      elsif Var'Tag = Gsl_String_Record'Tag then
         return "Gsl_String - " & Get_Value (Gsl_String (Var));

      elsif Var'Tag = Gsl_Boolean_Record'Tag then
         return "Gsl_Boolean - " & Get_Value (Gsl_Boolean (Var))'Img;

      elsif Var'Tag = Gsl_Natural_Record'Tag then
         return "Gsl_Natural - " & Get_Value (Gsl_Natural (Var))'Img;

      elsif Var'Tag = Gsl_List_Record'Tag then
         return "Gsl_List";

      elsif Var'Tag = Gsl_Var_Reference_Record'Tag then
         return "Gsl_Var_Reference";

      elsif Var'Tag = Gsl_Script_Reference_Record'Tag then
         return "Gsl_Script_Reference";

      else
         return "Unknown Type";
      end if;
   end Log_Gsl_Type;

end Giant.Gsl;
