------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-vis_windows.adb,v $, $Revision: 1.2 $
-- $Author: schwiemn $
-- $Date: 2003/06/03 17:18:38 $
--
package body Giant.Vis_Window_Management is


   --------------------------------------------------------------------------
   -- A
   -- Initialisation, Finalisation and Persistence
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Create_New_Empty_Vis_Window
     (Vis_Window_Name : in Valid_Names.Standard_Name)
     return Vis_Window_Data_Access;

   --------------------------------------------------------------------------
   procedure Vis_Window_Data_Access'Read
     (Stream : access Root_Stream_Type'Class;
      Item   : out    Vis_Window_Data_Access);

   -------------------------------------------------------------------------
   procedure Vis_Window_Data_Access'Write
     (Stream : access Root_Stream_Type'Class;
      Item   : in     Vis_Window_Data_Access);

   -------------------------------------------------------------------------
   procedure Deallocate_Vis_Window_Deep
     (Vis_Window : in out Visual_Window_Accsess);

   ---------------------------------------------------------------------------
   function Get_Vis_Window_Name
     (Vis_Window : in Visual_Window_Accsess)
     return String;

   ---------------------------------------------------------------------------
   function Equal
     (Left  : in Visual_Window_Data_Access;
      Right : in Visual_Window_Data_Access)
     return Boolean;

   ---------------------------------------------------------------------------
   function Less_Than
     (Left  : in Visual_Window_Data_Access;
      Right : in Visual_Window_Data_Access)
     return Boolean;


   ---------------------------------------------------------------------------
   -- B
   -- Management of the Selections that belong to a visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Selection_Exist
     (Vis_Window     : in Visual_Window_Data_Access;
      Selection_Name : in Valid_Names.Standard_Name)
     return Boolean;

   ---------------------------------------------------------------------------
   function Get_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name)
     return Graph_Lib.Selections.Selection_Access;

   ---------------------------------------------------------------------------
   function Get_All_Selection_Names_Sorted
     (Vis_Window : in Visual_Window_Data_Access)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   function Add_Selection
     (Vis_Window : in Visual_Window_Data_Access;
      Selection  : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   function Remove_Selection_From_Vis_Window
      (Vis_Window : in Visual_Window_Data_Access;
       Selection  : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return Graph_Lib.Selections.Selection_Access;

   ---------------------------------------------------------------------------
   procedure Set_Current_Selection
     (Vis_Window  : in Visual_Window_Accsess;
      Selection   : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return Graph_Lib.Selections.Selection_Access;

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
   -- C
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
   -- D
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
   -- Returns all Pins of a visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   -- Returns:
   --   The names of all pins sorted in ascending alphabetical
   --   order.
   function Get_All_Pin_Names_Sorted
     (Vis_Window : in Visual_Window_Data_Access)
     return String_Lists.List;
     
   ---------------------------------------------------------------------------  
   -- Adds a pin to a visualisation window.
   --  
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of the new pin.
   --   Pin_Data - The data describing the new pin.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_Does_Already_Exist_Exception - raised if "Pin_Name" does
   --     already exist.
   function Add_Pin
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name;
      Pin_Data   : in Vector);
      
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   Vektortyp von Steffen K.

   ---------------------------------------------------------------------------
   -- Removes a pin from a visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Raises 
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_With_Passed_Name_Not_Found_Exception - Raised if the
   --     pin "Pin_Name" is not found.
   function Remove_Pin_From_Vis_Window
      (Vis_Window : in Visual_Window_Data_Access;
       Pin_Name   : in Valid_Names.Standard_Name);


   ---------------------------------------------------------------------------
   -- E
   -- Visualisation Styles
   --
   -- Note a visualisation window knows only the name of its visualisation
   -- style. As visualisation styles are realized independently from
   -- project files and may be changed by the user, it is only garanted
   -- that after a visualisation window is loaded from its a
   -- visualsiation style with the stored name is set for this
   -- window. That does not garantee that the window nodes look
   -- like they have as the user stored the window, because in between
   -- the use may have changed the visualisation styles.
   -- If there is no appropriate visualisation style found then the
   -- Standard visualisation style will be used.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Raised if a demanded Vis_Style does not exist.
   Vis_Style_Does_Not_Exist : exception;

   ---------------------------------------------------------------------------
   -- Returns the name of the visualisation style assigned to this 
   -- visualisation window.
   --
   -- As already mentioned above it is not garanted that a 
   -- visualisation style with the returned name exists.
   -- 
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   -- Returns:
   --   The name of the visuliastion style assigned to the window. 
   function Get_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access)
     return String;

   ---------------------------------------------------------------------------
   -- Sets the name of the visualisation style of this visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Vis_Style - The name of a visualisation style.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Vis_Style_Does_Not_Exist - Raised if "Vis_Style" if not known
   --     by the package "Config.Vis_Styles".
   procedure Set_Vis_Window_Vis_Style
     (Vis_Window : in Visual_Window_Data_Access;
      Vis_Style  : in String);

------------------------------------------------------------------------------
private

   ---------------------------------------------------------------------------
   -- Management of the pins of a visualisation window.
   ---------------------------------------------------------------------------
   type Pin is record
     Pin_Name : String;
     Pin_Data : Vector;  --!!!!!!!!!!!!!!!!!!!!!     
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   Vektortyp von Steffen K.
   end record
   
   
   Pin_Equal (Left : in Pin; Right : in Pin) return Boolean;
   
   Pin_Less_Than (Left : in Pin; Right : in Pin) return Boolean;
   
   package pin_sets is new
   Ordered_Sets (Item_Type => Pin;
                 "="       => Pin_Equal;
                 "<"       => Pin_Less_Than);
						
   ---------------------------------------------------------------------------		
   -- Management of Selections inside a visualisation window
   ---------------------------------------------------------------------------
   
   type Selection_Data_Elemet is record
     Selection : Graph_Lib.Selections.Selection_Access;
     Selection_Highlight_Status : Highlight_Status;
     Is_Faded_Out : boolean;
     Is_Current_Selection : boolean;
     Is_Standard_Selection : boolean;
   end record;

   function Selection_Data_Equal
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean;

   function Selection_Data_Less_Than 
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean;

   -- needed for persistence
   procedure Selection_Data_Elemet'Write
     (Stream    : access Root_Stream_Type'Class;
      Selection : in Selection_Data_Elemet);

   -- needed for persistence
   procedure Selection_Data_Elemet'Read
     (Stream    : access Root_Stream_Type'Class;
      Selection : out Selection_Data_Elemet);


   package Selection_Data_Sets is new Ordered_Sets 
     (Item_Type => Selection_Data_Elemet,
      "="       => Selection_Data_Equal,
      "<"       => Selection_Data_Less_Than,
      Write     => Selection_Data_Elemet'Write,
      Read      => Selection_Data_Elemet'Read);


   ---------------------------------------------------------------------------
   -- The data model for a visualisation window
   ---------------------------------------------------------------------------
   
   type Visual_Window_Element;

   type Visual_Window_Accsess is access Visual_Window_Element;

   type Visual_Window_Element is record
   
      
      Vis_Window_Name        : Valid_Names.Standard_Name;

      The_Graph_Widget       : Graph_Widget;

      Set_Of_All_Pins        : Pin_Sets.Set;

      -- Aliases - also in the set All_Managed_Selections
      Standard_Selection     : Selection_Data_Elemet;

      -- Aliases - also in the set All_Managed_Selections
      Current_Selection      : Selection_Data_Elemet;

      -- A ordered set of all selections (including extra
      -- management data) that belong to this
      -- visulalisation window;
      All_Managed_Selections : Selection_Data_Sets.Set;

   end record;

end Giant.Vis_Window_Management;



