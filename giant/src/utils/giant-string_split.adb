------------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-string_split.adb,v $, $Revision: 1.4 $
--  $Author: koppor $
--  $Date: 2003/07/02 11:18:50 $
--

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Giant.String_Split is

   function Split_String
     (Source  : in String;
      Pattern : in String)
     return String_Lists.List
   is

      procedure Split_String
        (Source  : in     String;
         Pattern : in     String;
         List    : in out String_Lists.List)
      is
         I : Natural;
      begin
         I := Ada.Strings.Fixed.Index (Source, Pattern);

         if I < Source'First then
            -- Case for strings that do not hold a separator pattern;
            String_Lists.Attach
              (List,
               Ada.Strings.Unbounded.To_Unbounded_String (Source));
         else
            --  I >= Source'First
            --  Index has found the pattern

            if I = Source'First then
               --  if there is no string before pattern -> attach a null string
               --    (i.e. Source looks like ",X")
               String_Lists.Attach
                 (List,
                  Ada.Strings.Unbounded.Null_Unbounded_String);
            else
               --  attach the full found string
               String_Lists.Attach
                 (List,
                  Ada.Strings.Unbounded.To_Unbounded_String
                  (Source (Source'First .. I-1)));
            end if;

            if I = Source'Last then
               --  if there is no string at the end,
               --    (i.e. Source looks like "X,")
               --     attach a null string and finish
               String_Lists.Attach
                 (List,
                  Ada.Strings.Unbounded.Null_Unbounded_String);
            else
               --  there is more to split
               --    (i.e. Source looks like "X,Y"
               --  recursively split
               Split_String
                 (Source (I + Pattern'Length .. Source'Last),
                  Pattern,
                  List);
            end if;
         end if;

      end Split_String;

      Res : String_Lists.List;

   begin
      Res := String_Lists.Create;
      Split_String (Source, Pattern, Res);
      return Res;
   end Split_String;

end Giant.String_Split;
