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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-projects.adb,v $, $Revision: 1.1 $
--  $Author: schwiemn $
--  $Date: 2003/06/11 16:06:54 $
--
with Bauhaus_IO; -- from Bauhaus IML "Reuse.src"

package body Giant.Projects is

   ---------------------------------------------------------------------------
   --  0.1
   --  Streaming functionality for platform independent streams.
   ---------------------------------------------------------------------------
 
   --------------------------------------------------------------------------- 
   procedure Subgraph_Data_Elemet_Read
     (Stream  : in     Bauhaus_IO.In_Stream_Type;
      Element :    out Subgraph_Data_Elemet) is 
      
      Highlight_Integer_Id : Integer;
   begin
   
      Giant.Graph_Lib.Subgraphs.Subgraph_Read (Stream, Element.Subgraph);
      
      --  Read Highlight Status
      Bauhaus_IO.Read_Integer (Stream, Highlight_Integer_Id);
      Element.Highlight_Status := 
        Subgraph_Highlight_Status'Val (Highlight_Integer_Id);   
   end Subgraph_Data_Elemet_Read;
 
   ---------------------------------------------------------------------------
   procedure Subgraph_Data_Elemet_Write
     (Stream  : in Bauhaus_IO.In_Stream_Type;
      Element : in Subgraph_Data_Elemet) is 
      
      Highlight_Integer_Id : Integer;
   begin
   
      Giant.Graph_Lib.Subgraphs.Subgraph_Write (Stream, Element.Subgraph);
      
      --  Write Highlight Status (via Conversion to integer)
      Highlight_Integer_Id := 
        Subgraph_Highlight_Status'Pos (Element.Highlight_Status);   
      Bauhaus_IO.Write_Integer (Stream, Highlight_Integer_Id);
   end Subgraph_Data_Elemet_Read;
 

 
 
 
 
 
 
 
 
 
end Giant.Projects;


