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
--  $RCSfile: giant-main_window.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/06/16 21:48:30 $
--
--  Provides the main window. The main window is only instanciated once.
--
--  Pattern:
--    ADT
--

with Giant.Vis_Windows;
with Giant.Valid_Names;

package Giant.Main_Window is

   ---------------------------------------------------------------------------
   --  Window Methods
   ---------------------------------------------------------------------------

   procedure Add (Window_Name : Valid_Names.Standard_Name);

   procedure Update (Window_Name : Valid_Names.Standard_Name);

   procedure Remove (Window_Name : Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   --  Sets windows visible.
   --
   procedure Show;

   ---------------------------------------------------------------------------
   --  Quits the application.
   --
   procedure Quit;

end Giant.Main_Window;
