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
--  $RCSfile: giant-matrix_layouts.adb,v $, $Revision: 1.3 $
--  $Author: koppor $
--  $Date: 2003/07/02 11:59:11 $
--

with Giant.Graph_Lib;

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
      Res.Nodes_To_Layout := Graph_Lib.Node_Id_Sets.Copy
        (Graph_Lib.Selections.Get_All_Nodes
         (Selection_To_Layout));
      Res.Target_Position := Target_Position;
      Res.State           := Init;

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

      procedure Set_Position_Of_Top_Node
      is
      begin
         null;
      end Set_Position_Of_Top_Node;

   begin
      case Layout.State is
         when Init =>
           if Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) = 0 then
              Next_Action := Evolutions.Finish;
           elsif Graph_Lib.Node_Id_Sets.Size (Layout.Nodes_To_Layout) = 1 then
              --  Set node-position to Target_Position
              Next_Action := Evolutions.Finish;
           else
              --  real init
              Layout.State := Calc;
              Next_Action := Evolutions.Run;
           end if;

         when Calc =>
            Next_Action := Evolutions.Finish;
      end case;
   end Step;

end Giant.Matrix_Layouts;

