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
--  $RCSfile: giant-string_split.ads,v $, $Revision: 1.2 $
--  $Author: koppor $
--  $Date: 2003/07/07 09:52:21 $
--
------------------------------------------------------------------------------
--
--  Contains routine to split springs according to a defined pattern
--

with String_Lists;

package Giant.String_Split is

   ---------------------------------------------------------------------------
   --  Splits given string
   --
   --  Example:
   --      "a,b,,c", ","
   --    gets
   --      ("a", "b", "", "c")
   --
   --  Parameters:
   --    Source:  String to split
   --    Pattern: Separation character
   --    Trim:    If blanks should be trimmed at both sides
   --
   --  Returns:
   --    * List of substrings
   --    * Empty list, if source is empty
   function Split_String
     (Source  : in String;
      Pattern : in String;
      Trim    : in Boolean := false)
     return String_Lists.List;

end Giant.String_Split;
