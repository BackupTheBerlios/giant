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
--  $RCSfile: giant-vis_windows.adb,v $, $Revision: 1.5 $
--  $Author: schwiemn $
--  $Date: 2003/06/06 16:17:53 $
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
     
      New_Window_Ac     : Vis_Window_Data_Access;      
      New_Standard_Sel  : Graph_Lib.Selections.Selection;
      New_Graph_Widget  : Graph_Widgets.Graph_Widget;      
      Standard_Sel_Data : Selection_Data_Elemet;
   begin
   
      New_Window_Ac := new Visual_Window_Element;      
      New_Window_Ac.Vis_Window_Name := 
        Ada.Strings.Unbounded.To_Unbounded_String
          (Valid_Names.To_String (Vis_Window_Name));
   
      New_Window_Ac.Set_Of_All_Pins := Pin_Sets.Empty_Set;      
      New_Window_Ac.All_Managed_Selections := Selection_Data_Sets.Empty_Set;
      
      --  Initialize new Graph_Widget
      Graph_Widgets.Create (New_Graph_Widget);
      
      --  Increases the GTK Reference Counter - needed to keep the graph
      --  widget persistent in this data structure
      Gtk.Object.Ref (New_Graph_Widget);
            
      New_Window_Ac.The_Graph_Widget := New_Graph_Widget; 
                        
      --  Create empty standard selection and make it to the current selection
      --  -> this behaviour is demanded by the Specification
      --  (see Spec 3.4.2. Standard-Selektion).
      New_Standard_Sel := 
        Graph_Lib.Selections.Create (Standard_Selection_Name); 
                                 
      New_Window_Ac.Standard_Selection := 
        Ada.Strings.Unbounded.To_Unbounded_String 
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));    
      
      New_Window_Ac.Current_Selection :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Graph_Lib.Selections.Get_Name (New_Standard_Sel));      
      
      --  Build management data for selection
      Standard_Sel_Data.The_Selection := New_Standard_Sel;    
      Standard_Sel_Data.Selection_Highlight_Status := None;
      Standard_Sel_Data.Is_Faded_Out := False;

      -- Insert selection
      Selection_Data_Sets.Insert
        (New_Window_Ac.All_Managed_Selections, Standard_Sel_Data);  

      return New_Window_Ac;
   end Create_New_Empty_Vis_Window;

   ---------------------------------------------------------------------------
   procedure Vis_Window_Data_Access_Read
     (Stream : in  Bauhaus_IO.In_Stream_Type;
      Item   : out Vis_Window_Data_Access) is
   begin
   
      
    --  GRAPH_LIB; TODO
    --  !!!!!!!!!!
      
    null;  
      
   end Vis_Window_Data_Access_Read;
       
   ---------------------------------------------------------------------------
   procedure Vis_Window_Data_Access_Write
     (Stream : in Bauhaus_IO.In_Stream_Type;
      Item   : in Vis_Window_Data_Access) is   
   begin
   
    --  GRAPH_LIB; TODO
    --  !!!!!!!!!!
   
    null;
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
     
     Dummy_Selection    : Graph_Lib.Selections.Selection;
     Dummy_Data_Element : Selection_Data_Elemet;
     Found              : Boolean := False;
     
   begin

       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
       
       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);
       Dummy_Data_Element.The_Selection := Dummy_Selection;
       
       if Selection_Data_Sets.Is_Member
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element) then
          Found := True;
       end if;

       Graph_Lib.Selections.Destroy (Dummy_Selection);
       
       return Found;       
   end Does_Selection_Exist;

   ---------------------------------------------------------------------------
   function Get_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in Valid_Names.Standard_Name) 
      return Graph_Lib.Selections.Selection_Access is
      
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;
    
   begin

       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
       
       if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
          raise Selection_With_Passed_Name_Not_Found_Exception;
       end if;
       
       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
       Dummy_Data_Element.The_Selection := Dummy_Selection;
                   
       Return_Data_Element := Selection_Data_Sets.Get
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element);   
       Graph_Lib.Selections.Destroy (Dummy_Selection);
      
       return Return_Data_Element.The_Selection;
   end Get_Selection;

   ---------------------------------------------------------------------------
   function Get_All_Selections
     (Vis_Window : in Visual_Window_Data_Access)      
     return String_Lists.List is 
          
      The_List                 : String_Lists.List;      
      Set_Iter                 : Selection_Data_Sets.Iterator;
      A_Selection_Data_Element : Selection_Data_Elemet;
      
   begin
    
       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if; 
       
       The_List := String_Lists.Create;       
       Set_Iter := Selection_Data_Sets.Make_Iterator;
       
       -- append standard selection
       String_Lists.Append (The_List, Standard_Selection);
              
       while Selection_Data_Sets.More (Set_Iter) loop
       
          Selection_Data_Sets.Next (Set_Iter, A_Selection_Data_Element);
          
          -- do not append standard selection once again
          if not Ada.Strings.Unbounded."=" 
            (Graph_Lib.Selections.Get_Name 
              (A_Selection_Data_Element.The_Selection), Standard_Selection)
            then
            
            String_Lists.Append 
              (The_List, Ada.Strings.Unbounded.To_Unbounded_String 
                (Graph_Lib.Selections.Get_Name 
                  (A_Selection_Data_Element.The_Selection)));
           end if;      
       end loop;
             
       Selection_Data_Sets.Destroy (Set_Iter);
       
       return The_List;
   end Get_All_Selections;
     
   ---------------------------------------------------------------------------
   procedure Add_Selection
     (Vis_Window : in Visual_Window_Data_Access;
      Selection  : in Graph_Lib.Selections.Selection) is
      
      New_Sel_Data_Element : Selection_Data_Elemet;
      
   begin
    
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if; 
       
      if (Does_Selection_Exist 
        (Vis_Window, Valid_Names.To_Standard_Name 
          (Graph_Lib.Selections.Get_Name (Selection))) = True) then       
         raise Selection_Is_Already_Part_Of_Window_Exception;
      end if;

      -- Build management data for new selection
      New_Sel_Data_Element.The_Selection := Selection;
      Selection_Highlight_Status := None;
      Is_Faded_Out := False;

      Selection_Data_Sets.Insert
        (Vis_Window.All_Managed_Selections, New_Sel_Data_Element);      
   end if;

   ---------------------------------------------------------------------------
   procedure Remove_Selection_From_Vis_Window
     (Vis_Window     : in Visual_Window_Data_Access;
      Selection_Name : in Valid_Names.Standard_Name) is
       
     Dummy_Selection    : Graph_Lib.Selections.Selection;
     Dummy_Data_Element : Selection_Data_Elemet;
                      
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
       
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
       
      if Ada.Strings.Unbounded."=" 
        (Selection_Name, Vis_Window.Standard_Selection) then
         raise Standard_Selection_May_Not_Be_Removed_Exception;
      end if;
              
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
      Dummy_Data_Element.The_Selection := Dummy_Selection;
             
      Selection_Data_Sets.Remove
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);
    
      Graph_Lib.Selections.Destroy (Dummy_Selection);
             
      --  on removal of current selection the standard selection becomes
      --  the current selection
      if Ada.Strings.Unbounded."=" (Selection_Name, Current_Selection) then
         Vis_Window.Current_Selection := Vis_Window.Standard_Selection;
      end if;     
   end Remove_Selection_From_Vis_Window;
          
   ---------------------------------------------------------------------------
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return String is 
   begin
     
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
          
      return Ada.Strings.Unbounded.To_String (Vis_Window.Current_Selection);
   end Get_Current_Selection;

   ---------------------------------------------------------------------------
   procedure Set_Current_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name) is
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
       
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
      
      if Is_Faded_Out (Vis_Window, Selection_Name) = True then 
         raise Illegal_Current_Selection_Exception;
      end if;
   
      Vis_Window.Current_Selection := Selection_Name;      
   end Set_Current_Selection;  
      
   ---------------------------------------------------------------------------
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Data_Access) 
     return String is 
   begin  
                  
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
     
      return Ada.Strings.Unbounded.To_String (Vis_Window.Standard_Selection);          
   end Get_Standard_Selection;

   ---------------------------------------------------------------------------
   function Get_Highlight_Status
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in Valid_Names.Standard_Name);
     return Highlight_Status is
         
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;
    
   begin

       if Vis_Window = null then      
          raise Vis_Window_Data_Access_Not_Initialized_Exception;
       end if;
       
       if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
          raise Selection_With_Passed_Name_Not_Found_Exception;
       end if;
       
       Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
       Dummy_Data_Element.The_Selection := Dummy_Selection;
                   
       Return_Data_Element := Selection_Data_Sets.Get
         (Vis_Window.All_Managed_Selections, Dummy_Data_Element); 
           
       Graph_Lib.Selections.Destroy (Dummy_Selection);
      
       return Return_Data_Element.Selection_Highlight_Status;          
   end Get_Highlight_Status;

   ---------------------------------------------------------------------------
   function May_Highlight_Status_Be_Changed
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in Valid_Names.Standard_Name); 
     return Boolean is      
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
       
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
   
      -- check
      if Ada.Strings.Unbounded."=" 
        (Vis_Window.Current_Selection, Selection_Name) then
         return False;
      end if;
               
      return True;
   end May_Highlight_Status_Be_Changed;

   ---------------------------------------------------------------------------  
   procedure Set_Highlight_Status
     (Vis_Window           : in Visual_Window_Accsess;
      Selection_Name       : in Valid_Names.Standard_Name;
      New_Highlight_Status : in Highlight_Status) is
                 
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;            
   begin
  
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
       
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
      
      if May_Highlight_Status_Be_Changed (Vis_Window, Selection_Name) then
         raise Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception;
      end if;
                   
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
      Dummy_Data_Element.The_Selection := Dummy_Selection;
                   
      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element); 
                     
      Change_Data_Element.Selection_Highlight_Status := New_Highlight_Status;
           
      Graph_Lib.Selections.Destroy (Dummy_Selection);   
   end Set_Highlight_Status;

   --------------------------------------------------------------------------
   -- D
   -- Filters
   --------------------------------------------------------------------------
   
   --------------------------------------------------------------------------
   function May_Be_Faded_Out
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name)
     return Boolean is
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
       
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
   
      -- check
      if Ada.Strings.Unbounded."=" 
        (Vis_Window.Current_Selection, Selection_Name) then
         return False;
      end if;
   
      if Ada.Strings.Unbounded."=" 
        (Vis_Window.Standard_Selection, Selection_Name) then
         return False;
      end if;
   
      return True;
   end May_Be_Faded_Out;

   --------------------------------------------------------------------------
   function Is_Faded_Out
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name)
     return Boolean is   
                 
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Return_Data_Element : Selection_Data_Elemet;
       
   begin

      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
          
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
          
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
      Dummy_Data_Element.The_Selection := Dummy_Selection;
                    
      Return_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element); 
              
      Graph_Lib.Selections.Destroy (Dummy_Selection);
         
      return Return_Data_Element.Is_Faded_Out;          
   end Is_Faded_Out;

   --------------------------------------------------------------------------
   procedure Fade_Out_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name) is
      
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;    
      
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
          
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
      
      if (May_Be_Faded_Out (Vis_Window, Selection_Name) = False) then 
         raise Selection_May_Not_Be_Faded_Out_Exception;
      end if;
      
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
      Dummy_Data_Element.The_Selection := Dummy_Selection;                   
      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);                     
      Change_Data_Element.Is_Faded_Out := True;           
      Graph_Lib.Selections.Destroy (Dummy_Selection);      
   end Fade_Out_Selection;

   --------------------------------------------------------------------------
   procedure Fade_In_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name) is
      
      Dummy_Selection     : Graph_Lib.Selections.Selection;
      Dummy_Data_Element  : Selection_Data_Elemet;
      Change_Data_Element : Selection_Data_Elemet;    
      
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
          
      if (Does_Selection_Exist (Vis_Window, Selection_Name) = False) then
         raise Selection_With_Passed_Name_Not_Found_Exception;
      end if;
      
      if (Is_Faded_Out (Vis_Window, Selection_Name) = False) then 
         raise Selection_Is_Not_Faded_Out_Exception;
      end if;
      
      Dummy_Selection := Graph_Lib.Selections.Create (Selection_Name);  
      Dummy_Data_Element.The_Selection := Dummy_Selection;                   
      Change_Data_Element := Selection_Data_Sets.Get
        (Vis_Window.All_Managed_Selections, Dummy_Data_Element);                     
      Change_Data_Element.Is_Faded_Out := False;           
      Graph_Lib.Selections.Destroy (Dummy_Selection);     
   end Fade_In_Selection;      


   --------------------------------------------------------------------------
   -- E
   -- Pin Management
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Does_Exist
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Boolean is
     
      Dummy_Pin : Pin;
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
   
      Dummy_Pin.Pin_Name := Pin_Name;                         
      if Pin_Sets.Is_Member
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin) then          
         return True;
      end if;

      return False;   
   end Does_Pin_Exist;

   --------------------------------------------------------------------------
   function Get_Position
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Vis.Logic.Vector_2d is
     
     Dummy_Pin  : Pin;
     Return_Pin : Pin;
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
   
      if (Does_Exist (Vis_Window, Pin_Name) = False) then
         raise Pin_With_Passed_Name_Not_Found_Exception;
      end if;
   
      Dummy_Pin.Pin_Name := Pin_Name;                         
      
      Return_Pin := Pin_Sets.Get
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin);  
        
      return Return_Pin.Pin_Pos; 
   end Get_Position;
   
   --------------------------------------------------------------------------
   function Get_Zoom
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Vis.Zoom_Level is
     
     Dummy_Pin  : Pin;
     Return_Pin : Pin;
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
   
      if (Does_Exist (Vis_Window, Pin_Name) = False) then
         raise Pin_With_Passed_Name_Not_Found_Exception;
      end if;
   
      Dummy_Pin.Pin_Name := Pin_Name;                         
      
      Return_Pin := Pin_Sets.Get
        (Vis_Window.Set_Of_All_Pins, Dummy_Pin);  
        
      return Return_Pin.Pin_Zoom; 
   end Get_Zoom;  
            
   --------------------------------------------------------------------------
   function Get_All_Pins
     (Vis_Window : in Visual_Window_Data_Access)
     return String_Lists.List is
     
      The_List     : String_Lists.List;      
      Pin_Set_Iter : Pin_Sets.Iterator;
      A_Pin        : Pin;
     
   begin
     
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
  
      The_List := String_Lists.Create;       
      Pin_Set_Iter := Pin_Sets.Make_Iterator;
        
      while Pin_Sets.More (Pin_Set_Iter) loop      
         Pin_Sets.Next (Pin_Set_Iter, A_Pin);              
         String_Lists.Append (The_List, A_Pin.Pin_Name);               
      end loop;   
                
      Pin_Sets.Destroy (Pin_Set_Iter);
       
      return The_List;   
   end Get_All_Pins;
   
   ---------------------------------------------------------------------------  
   procedure Add_Pin
     (Vis_Window : in Visual_Window_Data_Access;
      Name       : in Valid_Names.Standard_Name;
      Position   : in Vis.Logic.Vector_2d
      Zoom_Level : in Vis.Zoom_Level) is
      
      New_Pin : Pin;
   begin
   
      if Vis_Window = null then      
         raise Vis_Window_Data_Access_Not_Initialized_Exception;
      end if;
      
      if (Does_Exist (Vis_Window, Pin_Name) = True) then
         raise Pin_Does_Already_Exist_Exception;
      end if;
      
      New_Pin.Pin_Name : Name;
      New_Pin.Pin_Pos  : Position;
      New_Pin.Pin_Zoom : Zoom_Level;
      
      Pin_Sets.Insert
        (Vis_Window.Set_Of_All_Pins, New_Pin);              
   end Add_Pin;
      
   ---------------------------------------------------------------------------
   function Remove_Pin_From_Vis_Window
      (Vis_Window : in Visual_Window_Data_Access;
       Pin_Name   : in Valid_Names.Standard_Name) is
       
   begin
   
   
   end Remove_Pin_From_Vis_Window;


   ---------------------------------------------------------------------------
   function Get_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access)
     return String;

   ---------------------------------------------------------------------------
   procedure Set_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access;
      Vis_Style  : in String);


end Giant.Vis_Window_Management;



