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
--  First Author: <unknown>
--
--  $RCSfile: template.ads,v $, $Revision: 1.1 $
--  $Author: squig $
--  $Date: 2003/05/23 15:31:19 $
--
------------------------------------------------------------------------------
--
--  Contains the GIANT source template.
--

with Giant.Logger;

package Giant.Template is

   ---------------------------------------------------------------------------
   --  Stores a foo bar.
   type Coordinate is private record;

   ---------------------------------------------------------------------------
   --  Raised on attempt show already visible window.
   Already_Visible_Exception : exception;

   ---------------------------------------------------------------------------
   --  Makes W visible on screen.
   --
   --  Parameters:
   --    W - The Window
   --  Returns:
   --    True, if successful; False, otherwise
   --  Raises:
   --    Already_Visible_Exception - raised if W is already visible
   function Show_Window
     (W : in Coordinate)
     return Booolean;

private

   type Coordinate is record
      --  X Coordinate
      X : Float;
      --  Y Coordinate
      Y : Float;
   end record;


end Giant.Template;
