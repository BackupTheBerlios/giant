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
--  $RCSfile: giant-matrix_layouts.adb,v $, $Revision: 1.2 $
--  $Author: koppor $
--  $Date: 2003/07/01 23:18:47 $
--

package body Giant.Matrix_Layouts is

   ---------------------------------------------------------------------------
   function Initialize
     (Widget              : in Giant.Graph_Widgets.Graph_Widget;
      Widget_Lock         : in Giant.Graph_Widgets.Lock_Type;
      Selection_To_Layout : in Giant.Graph_Lib.Selections.Selection;
      Target_Position     : in Giant.Vis.Logic.Vector_2d)
     return Matrix_Layout
   is
      Res : Matrix_Layout;
   begin
      Res                 := new Matrix_Layout_Record;
      Res.Widget          := Widget;
      Res.Widget_Lock     := Widget_Lock;
      Res.The_Selection   := Selection_To_Layout;
      Res.Target_Position := Target_Position;

      --  Evolutions.Initialize
      Initialize (Res);

      return Res;
   end Initialize;

   ---------------------------------------------------------------------------
   procedure Finish
     (Layout   : access Matrix_Layout_Record;
      Canceled : in     Boolean)
   is
   begin
      Graph_Widgets.Release_Lock (Layout.Widget, Layout.Widget_Lock);
   end Finish;

   ---------------------------------------------------------------------------
   procedure Step
     (Layout      : access Matrix_Layout_Record;
      Next_Action :    out Evolutions.Evolution_Action)
   is
   begin
      Next_Action := Evolutions.Finish;
   end Step;

end Giant.Matrix_Layouts;

