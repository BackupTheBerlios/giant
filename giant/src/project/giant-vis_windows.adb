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
--  $RCSfile: giant-vis_windows.adb,v $, $Revision: 1.4 $
--  $Author: schwiemn $
--  $Date: 2003/06/05 17:15:59 $
--
with Ada.Unchecked_Deallocation;

with Gtk.Object; -- from gtakada

with Unbounded_String_Hash; -- from Bauhaus IML "Reuse.src"

package body Giant.Vis_Window_Management is

   ---------------------------------------------------------------------------
   --  The default name used for standard selections
   --  Name must correspond Giant.Valid_Names.Standard_Name
   --  This constant is used to determine the name for a new standard 
   --  selection that has to be created during the execution of 
   --  "function Create_New ...".
   constant Standard_Selection_Name is Valid_Names.Standard_Name :=
     Valid_Names.To_Standard_Name ("Standard_Selection");


   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, Finalisation and Persistence
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Create_New
     (Vis_Window_Name : in Valid_Names.Standard_Name)
     return Vis_Window_Data_Access is
     
      New_Window_Ac    : Vis_Window_Data_Access;      
      New_Standard_Sel : Graph_Lib.Selections.Selection;
      New_Graph_Widget : Graph_Widgets.Graph_Widget;
      
   begin
   
      New_Window_Ac := new Visual_Window_Element;      
      New_Window_Ac.Vis_Window_Name := 
        Ada.Strings.Unbounded.To_Unbounded_String
          (Valid_Names.To_String (Vis_Window_Name));
   
      New_Window_Ac.Set_Of_All_Pins := Pin_Sets.Empty_Set;      
      New_Window_Ac.All_Managed_Selections := Selection_Data_Sets.Empty_Set;
      
      -- Initialize new Graph_Widget
      Graph_Widgets.Create (New_Graph_Widget);
      
      -- Increases the GTK Reference Counter - needed to keep the graph
      -- widget persistent in this data structure
      Gtk.Object.Ref (New_Graph_Widget);
            
      New_Window_Ac.The_Graph_Widget := New_Graph_Widget; 
                        
      -- Create empty standard selection and make it to the current selection
      -- -> this behaviour is demanded by the Specification
      -- (see Spec 3.4.2. Standard-Selektion).
      New_Standard_Sel := 
        Graph_Lib.Selections.Create (Standard_Selection_Name); 
                                 
      New_Window_Ac.Standard_Selection := 
        Ada.Strings.Unbounded.To_Unbounded_String 
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));    
      
      New_Window_Ac.Current_Selection :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));      
      
      Add_Selection (New_Window_Ac, New_Standard_Sel);
      
   end Create_New_Empty_Vis_Window;

   ---------------------------------------------------------------------------
   procedure Vis_Window_Data_Access_Read
     (Stream : in  Bauhaus_IO.In_Stream_Type;
      Item   : out Vis_Window_Data_Access) is
   begin
   
      
      GRAPH_LIB;
      !!!!!!!!!!
      
   end Vis_Window_Data_Access_Read;
       
   ---------------------------------------------------------------------------
   procedure Vis_Window_Data_Access_Write
     (Stream : in Bauhaus_IO.In_Stream_Type;
      Item   : in Vis_Window_Data_Access) is   
   begin
   
      GRAPH_LIB;
      !!!!!!!!!!
   
   end Vis_Window_Data_Access_Write;

   ---------------------------------------------------------------------------
   procedure Deallocate_Vis_Window_Deep
     (Vis_Window : in out Visual_Window_Access) is
     
      --------------------------------------------
      --  Deallocates all selections that are part of the elements stored
      --  in a Selection_Data_Set.
      procedure Deallocate_All_Selections_Part_Of_Elements_In_Set
        (The_Set : in Selection_Data_Sets.Set) is

         procedure Process_Element (Item : in Selection_Data_Elemet) is
         begin         
            Graph_Lib.Destroy (Selection_Data_Elemet.Selection);
         end Process_Element;

         procedure Deallocate_All_Selections is new
           Selection_Data_Sets.Apply (Execute => Process_Element);
        
      begin
    
         Deallocate_All_Selections (The_Set);
      end Insert_Edge_Class_Id_Set_Into_Class_Set;
   
      ---------------------------------------------
      procedure Free_Visual_Window_Access is new Ada.Unchecked_Deallocation
        (Visual_Window_Element, Visual_Window_Accsess);
     
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized;
      end if;
       
      --  Decreases the GTK Reference Counter.
      --  The Graph widget will be deallocated automatically 
      --  (only if there are no other references that have
      --  increased the Reference Counter).´  
      Gtk.Object.Unref (Visual_Window_Access.Graph_Widget);
      
      Pin_Sets.Destroy (Vis_Window.Set_Of_All_Pins);
      
      -- deep deallocation of all selections managed by the set.
      Deallocate_All_Selections_Part_Of_Elements_In_Set
        (Vis_Window.All_Managed_Selections);        
  
      Selection_Data_Sets.Destroy (Vis_Window.All_Managed_Selections);
      
      Free_Visual_Window_Access (Vis_Window);                        
   end Deallocate_Vis_Window_Deep;
     
   
   ---------------------------------------------------------------------------
   -- B
   -- General access on Visual_Window_Access
   ---------------------------------------------------------------------------
    
   ---------------------------------------------------------------------------
   function Get_Vis_Window_Name
     (Vis_Window : in Visual_Window_Accsess)
     return String is 
    
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
      
      return Ada.Strings.Unbounded.Unbounded_String 
        (Vis_Window.Vis_Window_Name);      
   end Get_Vis_Window_Name;
  
   ---------------------------------------------------------------------------
   function Is_Equal
     (Left  : in Visual_Window_Data_Access;
      Right : in Visual_Window_Data_Access)
     return Boolean is
          
   begin
   
      if (Left = null) or (Right = null) then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
      
      return Ada.Strings.Unbounded."=" 
        (Left.Vis_Window_Name, Right.Vis_Window_Name);   
   end Is_Equal;
   
   ---------------------------------------------------------------------------
   function Get_Hash_Value
     (Vis_Window  : in Visual_Window_Data_Access;
     return Integer is
     
   begin
     
      if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
     
      return function Unbounded_String_Hash (Vis_Window.Vis_Window_Name);         
   end Get_Hash_Value;


   ---------------------------------------------------------------------------
   -- C
   -- Management of the Selections that belong to a visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Selection_Exist
     (Vis_Window     : in Visual_Window_Data_Access;
      Selection_Name : in Valid_Names.Standard_Name)
     return Boolean is
     
     Dummy_Selection : Graph_Lib.Selections.Selection;
     Found : Boolean : False;
     
   begin

       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
       
       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
       
       if Selection_Data_Sets.Is_Member
         (Vis_Window.All_Managed_Selections, Dummy_Selection) then
          Found := True;
       end if;

       Graph_Lib.Selections.Destroy (Dummy_Selection);
       
       return False;       
   end Does_Selection_Exist;

   ---------------------------------------------------------------------------
   function Get_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in Valid_Names.Standard_Name) 
      return Graph_Lib.Selections.Selection_Access is
      
      Dummy_Selection : Graph_Lib.Selections.Selection;
      Return_Selection :  Graph_Lib.Selections.Selection;
    
   begin

       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
       
       if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
          raise Selection_With_Passed_Name_Not_Found_Exception;
       end if;
       
       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);              
       Return_Selection := Selection_Data_Sets.Get
         (Vis_Window.All_Managed_Selections, Dummy_Selection);   
       Graph_Lib.Selections.Destroy (Dummy_Selection);
      
       return Return_Selection;
   end Get_Selection;

   ---------------------------------------------------------------------------
   function Get_All_Selections
     (Vis_Window : in Visual_Window_Data_Access)      
     return String_Lists.List is 
          
      The_List : String_Lists.List;      
      Set_Iter : Selection_Data_Sets.Iterator;
     
   begin
    
       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if; 
       
       The_List := String_Lists.Create;       
       Set_Iter := Selection_Data_Sets.Make_Iterator;
       
       
       String_Lists.Append (The_List, 
       
       
       
       
       
       Selection_Data_Sets.Destroy (Set_Iter);
       
       return The_List;
   end Get_All_Selections;
     


   ---------------------------------------------------------------------------
   function Add_Selection
     (Vis_Window : in Visual_Window_Data_Access;
      Selection  : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   function Remove_Selection_From_Vis_Window
      (Vis_Window     : in Visual_Window_Data_Access;
       Selection_Name : in Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return String;

   ---------------------------------------------------------------------------
   procedure Set_Current_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return String;

   ---------------------------------------------------------------------------
   function Get_Highlight_Status_Of_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Highlight_Status;

   ---------------------------------------------------------------------------
   function May_Selection_Highlight_Status_Be_Changed
     (Vis_Window           : in Visual_Window_Accsess;
      Selection            : in Graph_Lib.Selections.Selection_Access); 
     return Boolean;

   ---------------------------------------------------------------------------  
   procedure Set_Highlight_Color_Of_Selection
     (Vis_Window           : in Visual_Window_Accsess;
      Selection            : in Graph_Lib.Selections.Selection_Access;
      New_Highlight_Status : in Highlight_Status);


   --------------------------------------------------------------------------
   -- D
   -- Filters
   --------------------------------------------------------------------------
   
   --------------------------------------------------------------------------
   function  May_Selection_Be_Faded_Out
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Boolean;

   --------------------------------------------------------------------------
   function Is_Selection_Faded_Out
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Boolean;

   --------------------------------------------------------------------------
   procedure Fade_Out_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access));

   --------------------------------------------------------------------------
   procedure Fade_In_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access));


   --------------------------------------------------------------------------
   -- E
   -- Pin Management
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Does_Pin_Exist
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Boolean;

   --------------------------------------------------------------------------
   function Get_Pin_Data
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Vector;
    
   --------------------------------------------------------------------------
   function Get_All_Pin_Names_Sorted
     (Vis_Window : in Visual_Window_Data_Access)
     return String_Lists.List;
     
   ---------------------------------------------------------------------------  
   function Add_Pin
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name;
      Pin_Data   : in Vector);
      
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   Vektortyp von Steffen K.

   ---------------------------------------------------------------------------
   function Remove_Pin_From_Vis_Window
      (Vis_Window : in Visual_Window_Data_Access;
       Pin_Name   : in Valid_Names.Standard_Name);


   ---------------------------------------------------------------------------
   function Get_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access)
     return String;

   ---------------------------------------------------------------------------
   procedure Set_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access;
      Vis_Style  : in String);


end Giant.Vis_Window_Management;



