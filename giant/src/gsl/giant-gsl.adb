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
-- $Date: 2003/06/09 14:17:15 $
--
-- This package implements the datatypes used in GSL.
--

with Ada.Tags;
use  Ada.Tags;

with Giant.Gsl.Types;
use  Giant.Gsl.Types;
with Giant.Gsl.Syntax_Tree;
use  Giant.Gsl.Syntax_Tree;

package body Giant.Gsl is

   procedure Log_Syntax_Node
     (Node : Syntax_Node) is

      L  : Gsl_Type;
      --LS : Gsl_String;
      --LN : Gsl_Natural;
      --LR : Gsl_Var_Reference;
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
                     --LS := Gsl_String (L);
                     --Text_IO.Put_Line (Get_Value (LS));
                  elsif L'Tag = Gsl_Natural_Record'Tag then
                     Default_Logger.Debug
                       ("Syntax_Node LITERAL = Natural", "Giant.Gsl");
                     --LN := Gsl_Natural (L);
                     --Text_IO.Put_Line (Get_Value (LN)'Img);
                  end if;
               end if;

            when Visible_Var =>
               Default_Logger.Debug
                 ("Syntax_Node VISIBLE_VAR", "Giant.Gsl");

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

end Giant.Gsl;
