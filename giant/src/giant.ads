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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/09/09 20:22:24 $
--
------------------------------------------------------------------------------
--
--  Contains the almost empty root package.
--
--  Do not add any code.
--

package Giant is

   ---------------------------------------------------------------------------
   --  Returns the localized value of S.
   --
   --  Returns:
   --    The localized value if known; S, otherwise
   function "-" (S : String) return String;

end Giant;
