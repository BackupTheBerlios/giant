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
-- $RCSfile: giant-vis_windows.ads,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 08:56:23 $
--
-- ----------------
-- This package realizes a container that administrates the components
-- that decribe a visualisation window, i.e. this is the data model
-- for a visualisation_window.
--
-- Each Container is an Abstract Data Type (ADT), the administrated
-- Components are selections, pins and a graph_widget.
-- The container holds all data needed to sava a visual window
-- persistent into a file.
--
-- This "data model" is absolute independent from any kind
-- of a graphical represenation of a visualiasation window.
-- --> Zhe user of this package has to take care for the
-- consistency between this "data model" and any graphical
-- representation.
--
-- The purpose of this package is to combine all components
-- describing the not redundant data needed for the persistence of
-- a visualisation window.
--
with String_Lists; -- from Bauhaus IML "Reuse.src"

with Giant.Valid_Names;              -- from GIANT
with Giant.Graph_Lib.Selections;     -- from GIANT
with Giant.Graph_Lib.Selection_Sets; -- from GIANT


package Giant.Vis_Window_Management is

   -- Dummy
   type Graph_Widget is Integer;

   --------------------------------------------------------------------------
   -- The ADT offered by this package.
   -- A Pointer to a data object that describes a visual window
   type Visual_Window_Data_Access is private;

   --------------------------------------------------------------------------
   -- This type describes the colors that may be used to highlight all
   -- selections except the current selection.
   type Selection_Highlight_Status is (None, Color_1, Color_2, Color_3);

   --------------------------------------------------------------------------
   -- This exception is raised if a not initialized instance of the
   -- ADT "Visual_Window_Data_Access" is passed as parameter to one
   -- of the subprograms in this package.
   Vis_Window_Data_Access_Not_Initialized_Exception : exception;


   --------------------------------------------------------------------------
   -- A
   -- Initialisation, Finalisation and Persistence
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   -- This subprogram initializes the ADT Vis_Window_Data_Access
   -- by creating a new empty instance of the data model for
   -- visualisation windows.
   --
   -- There is one empty standard selection, this is also
   -- the current selection.
   -- The name of the visualisation window is given as parameter.
   --
   -- Parameters:
   --   Vis_Window_Name - The name of the visualisation window.
   -- Returns:
   --   A pointer that points to a new data object describing a
   --   visualisation window
   function Create_New_Empty_Vis_Window
     (Vis_Window_Name : in Valid_Names.Standard_Name)
     return Vis_Window_Data_Access;

   --------------------------------------------------------------------------
   -- This subprogram initializes the ADT by creating a new instance
   -- based on the data read from the stream.
   --
   -- Parameters:
   --   Stream - the stream where the data is read.
   --   Item - the new Instance of the ADT.
   procedure Vis_Window_Data_Access'Read
     (Stream : access Root_Stream_Type'Class;
      Item   : out    Vis_Window_Data_Access);

   -------------------------------------------------------------------------
   -- This method writes the Container including all its components
   -- into a stream.
   --
   -- Parameters:
   --   Stream - the stream into that the data should be written.
   --   Item - The Instance of the ADT that should be written into
   --     a stream.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   procedure Vis_Window_Data_Access'Write
     (Stream : access Root_Stream_Type'Class;
      Item   : in     Vis_Window_Data_Access);

   -------------------------------------------------------------------------
   -- Deallocates the ADT.
   --
   -- Note:
   --   This procedure performs a DEEP DEALLOCATIOMN,
   --   i.e. all selections that belong to
   --   that visualisation window are deallocated too.
   --   As Selections are realized as pointers you should
   --   beware of dangling pointers.
   --
   -- Parameters:
   --   Vis_Window - the instance of the ADT that should be deallocated.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   procedure Deallocate_Vis_Window_Deep
     (Vis_Window : in out Visual_Window_Accsess);

   ---------------------------------------------------------------------------
   -- Returns the name of a visualisation window.
   --
   -- Parameters:
   --   Visual_Window - The instance of the ADT whose name should
   --     be returned.
   -- Returns:
   --   The name of the visualisation window as a unbounded string.
   --
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Get_Vis_Window_Name
     (Vis_Window : in Visual_Window_Accsess)
     return String;

   ---------------------------------------------------------------------------
   -- Equal function - to instances of the ADT having the
   -- same name are regarded as equal. This is necessary because of the
   -- fact, that each visualisation window of a project must have a
   -- unique name.
   --
   -- This function is used to organize instances of the ADT
   -- in ordered sets (package ordered_sets from Bauhaus Reuse.src).
   --
   -- It is garanted that:
   -- (Equal (Vis_Window_A, Vis_Window_B) = TRUE) if and only if
   -- (Get_Vis_Window_Name(Vis_Window_A) = Get_Vis_Window_Name(Vis_Window_B))
   --
   -- Parameters:
   --   Left : An instance of the ADT.
   --   Right: An instance of the ADT:
   -- Returns:
   --   True, if the two instances are equal; False, otherwise.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Equal
     (Left  : in Visual_Window_Data_Access;
      Right : in Visual_Window_Data_Access)
     return Boolean;

   ---------------------------------------------------------------------------
   -- "Less Than" Function for the ADT.
   --
   -- This function is used to organize instances of the ADT
   -- in ordered sets (package ordered_sets from Bauhaus Reuse.src).
   --
   -- It is garanted that:
   -- Visual_Window_Data_Access_A < Visual_Window_Data_Access_A
   -- if and only if
   -- Get_Vis_Window_Name(Window_A) < Get_Vis_Window_Name(Window_B)
   --
   -- Parameters:
   --   Left : An instance of the ADT.
   --   Right: An instance of the ADT:
   -- Returns:
   --   True, if Left < Right; False, otherwise (Left >= Right);
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Less_Than
     (Left  : in Visual_Window_Data_Access;
      Right : in Visual_Window_Data_Access)
     return Boolean;


   ---------------------------------------------------------------------------
   -- B
   -- Management of the Selections that belong to a visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This exception is raised if a selection with a passed name is not found.
   Selection_With_Passed_Name_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- This exception is raised if a selection
   -- (Graph_Lib.Selections.Selection_Access) is not found.
   Selection_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- Each selection must have a unique name - this exception is raised
   -- if subprogram call will violate this principle.
   Selection_Is_Already_Part_Of_Window_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised on trial to remove the standard selection
   Standard_Selection_May_Not_Be_Removed_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised on Trial to change the Highlight-Status of a selection if
   -- that is not allowed.
   Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised on attempt to fade out a selection that can not be faded out.
   Selection_May_Not_Be_Faded_Out_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised on attempt to fade in a selection that is not faded out.
   Selection_Is_Not_Faded_Out_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Checks whether the data model vor a visualisation window ha a
   -- selection with the given name.
   --
   -- Parameters:
   --   Visual_Window - An instance of the ADT.
   --   Selection_Name - The name of a selection.
   -- Returns:
   --   True, if "Vis_Window" has a selection with the given name
   --   "Selection_Name"; False, otherwise.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter
   function Does_Selection_Exist
     (Vis_Window     : in Visual_Window_Data_Access;
      Selection_Name : in Valid_Names.Standard_Name)
     return Boolean;

   ---------------------------------------------------------------------------
   -- This function returns a selection.
   --
   -- Note
   --   As only a pointer to a "selection data object" is returned
   --   several calls of this function with the same parameters will
   --   cause aliases.
   --
   --   You may NOT DEALLOCATE the returned Selection
   --   before it is removed from the Visualisation Window
   --   by calling "Remove_Selection_From_Vis_Window".
   --
   -- Parameters:
   --   Vis_Window - The "model" for visual window to that the selection
   --     belongs.
   --   Selection_Name - The name of the searched selection.
   -- Returns:
   --   The selection with the given name.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_With_Passed_Name_Not_Found_Exception - Raise if
   --     "Vis_Window" has no selection with the name "Selection_Name".
   function Get_Selection
     (Vis_Window     : in Visual_Window_Accsess;
      Selection_Name : in Valid_Names.Standard_Name)
     return Graph_Lib.Selections.Selection_Access;

   ---------------------------------------------------------------------------
   -- Returns a list of a all selections that belong to this model for a
   -- visualisation window.
   -- The standard selection is the first selection in this list.
   -- The other selections are sorted in ascending alphabetical order
   -- according to the selection names.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   -- Returns:
   --   A list holding the names of all known selctions.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Get_All_Selection_Names_Sorted
     (Vis_Window : in Visual_Window_Data_Access)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Adds a Selection to the visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for visual window.
   --   Selection  - The selection that should be added.
   -- Returns:
   --   A list about all selections.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Is_Already_Part_Of_Window_Exception - Raised if there
   --     is already a selection with the same name in "Vis_Window".
   function Add_Selection
     (Vis_Window : in Visual_Window_Data_Access;
      Selection  : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   -- Removes a selection.
   -- Selection is only removed from the visualisation window
   -- NO DEALLOCATION is done for the selection.
   -- After the call of that subprogram a selection may be deallocated
   -- without affeting Vis_Window.
   --
   -- The Standard Selection may not be removed.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection  - The selection that should be removed.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_With_Passed_Name_Not_Found_Exception - Raise if
   --     "Vis_Window" has no selection with the name "Selection_Name".
   --   Standard_Selection_May_Not_Be_Removed_Exception - Raised on Trail
   --     to remove the standard selection.
   --   Selection_Not_Found_Exception - Raised if the selection "Selection"
   --     is not part of "Visual_Window_Data_Access".
   function Remove_Selection_From_Vis_Window
      (Vis_Window : in Visual_Window_Data_Access;
       Selection  : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   -- Returns the name of the current selection.
   -- There is always an current selection.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window whose current
   --     selection should be returned.
   -- Returns:
   --   The current seclection.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return Graph_Lib.Selections.Selection_Access;

   ---------------------------------------------------------------------------
   -- Sets the current selection.
   -- The old current Selection will loose the status "current selection"
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window whose current
   --     selection should be changed.
   --   Selection  - The selection that should be the new current selection.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exception - Raised if the selection "Selection"
   --     is not part of "Visual_Window_Data_Access".
   procedure Set_Current_Selection
     (Vis_Window  : in Visual_Window_Accsess;
      Selection   : in Graph_Lib.Selections.Selection_Access);

   ---------------------------------------------------------------------------
   -- Returns the standard selection.
   -- There is always a standard selection.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window whose standard
   --     selection should be returned.
   -- Returns:
   --   The standard selection.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Data_Access)
     return Graph_Lib.Selections.Selection_Access;

   ---------------------------------------------------------------------------
   -- Returns the Highlight-Status of a selection.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - The selection whose "Highlight-Status" is
   --     returned.
   -- Returns:
   --   The highlight status.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exceptionv - Raised if the passed selection
   --     is not part of "Vis_Window".
   function Get_Highlight_Status_Of_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Highlight_Status;

   ---------------------------------------------------------------------------
   -- Determines whether the "Highlight-Status" of a selection may be changed.
   -- This is necessary as you are not allowed to change the highlight status
   -- of the current selection.
   --
   -- Parameters:   
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - The selection whose "Highlight-Status" should be
   --     changed.
   -- Returns: 
   --   True, if the "Highlight-Status" may be changed; False, otherwise.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exceptionv - Raised if the passed selection
   --     is not part of "Vis_Window".
   function May_Selection_Highlight_Status_Be_Changed
     (Vis_Window           : in Visual_Window_Accsess;
      Selection            : in Graph_Lib.Selections.Selection_Access); 
     return Boolean;

   ---------------------------------------------------------------------------
   -- Changes the "Higlight-Status" of a selection.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - The selection whose "Highlight-Status" is
   --     changed.
   --   New_Highlight_Status - The new Highlight-Status for the "Selection".
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exceptionv - Raised if the passed selection
   --     is not part of "Vis_Window".
   --   Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception - raised
   --     if it is not allowed to change the Highlight-Status of
   --     "Selection".   
   procedure Set_Highlight_Color_Of_Selection
     (Vis_Window           : in Visual_Window_Accsess;
      Selection            : in Graph_Lib.Selections.Selection_Access;
      New_Highlight_Status : in Highlight_Status);

   --------------------------------------------------------------------------
   -- C
   -- Filters
   --------------------------------------------------------------------------
   
   --------------------------------------------------------------------------
   -- Determines whether it is possible to fade out a selection.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - A selection.
   -- Returns:
   --   True, if the selection may be faded out; False, otherwise.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exceptionv - Raised if the passed selection
   --     is not part of "Vis_Window".
   function  May_Selection_Be_Faded_Out
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Boolean;

   --------------------------------------------------------------------------
   -- Determines whether a selection is faded out.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - A selection.
   -- Returns:
   --   True, if the selection is faded out; False, otherwise. 
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exception - Raised if the passed selection
   --     is not part of "Vis_Window".
   function Is_Selection_Faded_Out
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access)
     return Boolean;

   --------------------------------------------------------------------------
   -- Fades a selection out.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - A selection.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exception - Raised if the passed selection
   --     is not part of "Vis_Window".
   --  Selection_May_Not_Be_Faded_Out_Exception - Raised if "Selection"
   --    may not be faded out.
   procedure Fade_Out_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access));

   --------------------------------------------------------------------------
   -- Fades in a selection (only selections that are currently faded out).
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection - A selection.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_Not_Found_Exception - Raised if the passed selection
   --     is not part of "Vis_Window".
   --   Selection_Is_Not_Faded_Out_Exception - Raised if "Selection" is
   --     currently not faded out.
   procedure Fade_In_Selection
     (Vis_Window : in Visual_Window_Accsess;
      Selection  : in Graph_Lib.Selections.Selection_Access));


   --------------------------------------------------------------------------
   -- D
   -- Pin Management
   -- Provides functionality for the managent of pins.
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   -- Raised if a pin with the passed name does not exist.     
   Pin_With_Passed_Name_Not_Found_Exception : exception;
   
   --------------------------------------------------------------------------
   -- Raised if a added pin already exists.
   Pin_Does_Already_Exist_Exception : exception;

   --------------------------------------------------------------------------
   -- Determines whether a Pin with the given name exists.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Returns:
   --   True, if the pin exists; False, otherwise.
   -- Raises:
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Does_Pin_Exist
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Boolean;

   --------------------------------------------------------------------------
   -- Returns a Pin.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Returns:
   --   The Data for the position of the visual window content
   --   stored in a pin.
   -- Raises
   --   Vis_Window_Data_Access_Not_Initialized - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_With_Passed_Name_Not_Found_Exception - Raised if the
   --     pin "Pin_Name" is not found.
   function Get_Pin_Data
     (Vis_Window : in Visual_Window_Data_Access;
      Pin_Name   : in Valid_Names.Standard_Name)
     return Vector;
    
    
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   Vektortyp von Steffen K.
     
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

      Vis_Window_Name       : Valid_Names.Standard_Name;

      The_Graph_Widget      : Graph_Widget;

      Set_Of_All_Pins       : Pin_Sets.Set;

      -- Aliases - also in the set All_Managed_Selections
      Standard_Selection    : Selection_Data_Elemet;

      -- Aliases - also in the set All_Managed_Selections
      Current_Selection      : Selection_Data_Elemet;

      -- A ordered set of all selections (including extra
      -- management data) that belong to this
      -- visulalisation window;
      All_Managed_Selections : Selection_Data_Sets.Set;

   end record;

end Giant.Vis_Window_Management;



