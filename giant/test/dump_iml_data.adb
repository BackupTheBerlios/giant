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
--  $RCSfile: dump_iml_data.adb,v $, $Revision: 1.4 $
--  $Author: schwiemn $
--  $Date: 2003/07/10 12:27:21 $
--  
-- -----
-- Used to Dump the Content (Node Classes and attributes of an iml Graph)
-- into the logger.
--
with Giant.Graph_Lib; 
with Giant.Logger; 
with Giant.Default_Logger;

procedure dump_iml_data is 

   package Logger is new Giant.Logger("dump_iml_data");

   Node_Classes      : Giant.Graph_Lib.Node_Class_Id_Set;
   Node_Classes_Iter : Giant.Graph_Lib.Node_Class_Id_Sets.Iterator;
   A_Node_Class      : Giant.Graph_Lib.Node_Class_Id;

   Attributes_Iter : Giant.Graph_Lib.Node_Attribute_Iterator;
   A_Attribute     : Giant.Graph_Lib.Node_Attribute_Id;
   
   Edge_Attr_Set  : Giant.Graph_Lib.Edge_Class_Id_Set;
   Edge_Attr_Iter : Giant.Graph_Lib.Edge_Class_Id_Sets.Iterator;
   A_Edge_Attr    : Giant.Graph_Lib.Edge_Class_Id;
   
   Node_Class_Counter : Integer;
   Edge_Class_Counter : Integer;
   Attr_Counter       : Integer;

begin
   Giant.Graph_Lib.Initialize;
   
   Giant.Default_Logger.Init ("a_iml_dump.log");
   Logger.Info ("Starting Data Dump ...");
         
   Node_Classes := Giant.Graph_Lib.Get_All_Node_Class_Ids;   

     
   -- All node classes
   -------------------
   Logger.Info ("");
   Logger.Info ("");
   Logger.Info ("All Node Classes of the iml graph:");
   Logger.Info ("----------------------------------");
 
   Node_Classes_Iter := 
     Giant.Graph_Lib.Node_Class_Id_Sets.Make_Iterator (Node_Classes);
  
   Node_Class_Counter := 1;   
   while Giant.Graph_Lib.Node_Class_Id_Sets.More (Node_Classes_Iter) loop
  
      Giant.Graph_Lib.Node_Class_Id_Sets.Next 
        (Node_Classes_Iter, A_Node_Class);
    
      Logger.Info
        (Integer'Image (Node_Class_Counter) & ". " & "Node Class: """
         & Giant.Graph_Lib.Get_Node_Class_Tag (A_Node_Class)
         & """"); 
         
      Node_Class_Counter := Node_Class_Counter + 1;
   end loop; 
   
   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes_Iter); 
   
   
   -- All edge classes
   -------------------   
   Logger.Info ("");
   Logger.Info ("");
   Logger.Info ("All Edge Classes of the iml graph:");
   Logger.Info ("----------------------------------");
   
   Edge_Attr_Set := 
     Giant.Graph_Lib.Get_All_Edge_Class_Ids;
   
   Edge_Attr_Iter := 
     Giant.Graph_Lib.Edge_Class_Id_Sets.Make_Iterator (Edge_Attr_Set);
     
   Edge_Class_Counter := 1;
   while Giant.Graph_Lib.Edge_Class_Id_Sets.More (Edge_Attr_Iter) loop
                  
      Giant.Graph_Lib.Edge_Class_Id_Sets.Next 
        (Edge_Attr_Iter, A_Edge_Attr);
                      
      Logger.Info
        (Integer'Image (Edge_Class_Counter) & ". " & "Edge Class: """              
         & Giant.Graph_Lib.Get_Edge_Class_Tag 
             (A_Edge_Attr)
         & """");  
         
      Edge_Class_Counter := Edge_Class_Counter + 1;      
   end loop;       
    
   Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Set);
   Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Iter);   
      
      
   -- Details for each node class
   ------------------------------   
   Logger.Info ("");
   Logger.Info ("");  
   Logger.Info ("Details about node classes of the iml graph:");
   Logger.Info ("--------------------------------------------");
   
   Node_Classes_Iter := 
     Giant.Graph_Lib.Node_Class_Id_Sets.Make_Iterator (Node_Classes);
   
   Node_Class_Counter := 1;  
   while Giant.Graph_Lib.Node_Class_Id_Sets.More (Node_Classes_Iter) loop
  
      Giant.Graph_Lib.Node_Class_Id_Sets.Next 
        (Node_Classes_Iter, A_Node_Class);

      Logger.Info ("");  
      Logger.Info ("");      
      Logger.Info
        (Integer'Image (Node_Class_Counter) & ". " & "Node Class: """
         & Giant.Graph_Lib.Get_Node_Class_Tag (A_Node_Class)
         & """");
      Logger.Info ("");      
          
            
      -- 1. Dump all attributes
      -------------------------
      Attributes_Iter := Giant.Graph_Lib.Make_Attribute_Iterator
        (A_Node_Class);
                
      Attr_Counter := 1;
      while Giant.Graph_Lib.More (Attributes_Iter) loop
      
         Giant.Graph_Lib.Next (Attributes_Iter, A_Attribute);
         
         Logger.Info
           ("  " & Integer'Image (Attr_Counter) & ". " & "Attribute: """                 
            & Giant.Graph_Lib.Convert_Node_Attribute_Id_To_Name
                (A_Attribute)
            & """");     
         Attr_Counter := Attr_Counter + 1; 
            
      end loop;
     
      Logger.Info ("");  
                   
                   
      -- 2. Dump all edge attributes (subset of  1.)
      ----------------------------------------------      
      Edge_Attr_Set := 
        Giant.Graph_Lib.Get_All_Edge_Class_Ids_For_Node_Class (A_Node_Class);
   
      Edge_Attr_Iter := 
        Giant.Graph_Lib.Edge_Class_Id_Sets.Make_Iterator (Edge_Attr_Set);
        
      Edge_Class_Counter := 1;
      while Giant.Graph_Lib.Edge_Class_Id_Sets.More (Edge_Attr_Iter) loop
      
         Giant.Graph_Lib.Edge_Class_Id_Sets.Next 
           (Edge_Attr_Iter, A_Edge_Attr);
                      
         Logger.Info
           ("  " & Integer'Image (Edge_Class_Counter) & ". " & "Edge_Attr: """                
            & Giant.Graph_Lib.Get_Edge_Class_Tag 
                (A_Edge_Attr)
            & """");
            
         Edge_Class_Counter := Edge_Class_Counter + 1;          
      end loop;
            
      Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Set);
      Giant.Graph_Lib.Edge_Class_Id_Sets.Destroy (Edge_Attr_Iter);        
      
      Node_Class_Counter := Node_Class_Counter + 1;
   end loop;      

   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes_Iter);    
   Giant.Graph_Lib.Node_Class_Id_Sets.Destroy (Node_Classes);   
   Giant.Graph_Lib.Destroy;
end dump_iml_data;

