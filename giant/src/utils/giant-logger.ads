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
--  $RCSfile: giant-logger.ads,v $, $Revision: 1.5 $
--  $Author: keulsn $
--  $Date: 2003/06/01 22:06:52 $
--
------------------------------------------------------------------------------
--
--  Contains the generic logging package.
--
--  To use this package, first create an instance of this package:
--
--    package My_Logger is new Logger("giant.mypackage");
--
--  Then call one of the methods depending on the severity:
--
--    My_Logger.Debug ("debug message");
--
--  See:
--    Giant.Default_Logger

generic

   ---------------------------------------------------------------------------
   --  The name of the logger. Use a hierarchial name like
   --  giant.mypackage. Log messages can be filtered by this name.
   Name : String;

package Giant.Logger is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Debug
   procedure Debug (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Error
   procedure Error (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Fatal
   procedure Fatal (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Info
   procedure Info (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Warn
   procedure Warn (Message : in String);

end Giant.Logger;

