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
-- $RCSfile: giant-projects.ads,v $, $Revision: 1.7 $
-- $Author: schwiemn $
-- $Date: 2003/06/12 18:01:41 $
--
-- --------------------
-- This package provides an ADT which acts as a container for all
-- the components forming a project.
-- This components are visualisation windows, node annotations and
-- iml subgraphs.
--
-- This package controls the management files for every component
-- of a project and the project file itself.
-- Therefore this package is solely responisble for the persistence of
-- projects - not any other package.
--
-- In order to avoid a cyclic package dependency some functionality
-- regarding the node_annotations has been put into the package
-- "Node_Annotation_Management".
--
--with Ada.Text_IO;
--with Ada.Strings.Unbounded;

--with Giant.Graph_Lib;                  -- from GIANT
--with Giant.Graph_Lib.Subgraphs;        -- from GIANT
--with Giant.Valid_Names;                -- from GIANT
--with Giant.Vis_Window_Sets;            -- from GIANT
--with Giant.Vis_Window_Management;      -- from GIANT
--with Giant.Node_Annotation_Management; -- from GIANT

--with String_Lists; -- from Bauhaus IML "Reuse.src"

package Giant.Projects is

   ---------------------------------------------------------------------------
   -- This ADT realizes a GIANT project
   -- It contains all data needed to describe
   -- a project.
   -- It is implemented as a pointer - Aliasing effects are possible
   type Project_Access is private;

   ---------------------------------------------------------------------------
   -- Describes the highlight status of an subgraph
   type Subgraph_Highlight_Status : exception;

   ---------------------------------------------------------------------------
   -- Raised if a passed parameter of type Project_Access is not
   -- initialized.
   Project_Access_Not_Initialized_Exception : exception;


   ---------------------------------------------------------------------------
   -- A
   -- General Project Management
   --
   -- This part describes the functionlity needed to deal with projects.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Each project directory may only hold one project file.
   Directory_Holds_Already_A_Project_File_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a passed directory is not found or could not be accessed
   Invalid_Project_Directory_Excpetion : exception;

   ---------------------------------------------------------------------------
   -- Raised if a project with the passed name does not exist.
   Project_Does_Not_Exist_Exception : exception;
   
   ---------------------------------------------------------------------------
   -- Raised on attempt to load a project while the "giant.graph_lib" holds 
   -- an iml graph that has a different check sum. 
   Wrong_IML_Graph_Loaded_Exception : exception;


   ---------------------------------------------------------------------------
   -- Determines whether a project exists.
   --
   -- Parameters:
   --   Project_Name - The name of a project.
   --   Project_Directory - The directory of the project. Only absolute paths
   --     should be passed.
   -- Retruns:
   --   True, if a project file with name "Project_Name" is loacted
   --   in the directory "Project_Directory"; False, otherwise.
   -- Raises:
   --   Invalid_Project_Directory_Excpetion - Raised if the passed directory
   --     "Project_Directory" is not found or could not be accessed.
   function Does_Project_Exist
     (Project_Name      : in Valid_Names.Standard_Name;
      Project_Directory : in String)
      return Boolean;

   ---------------------------------------------------------------------------
   -- Determines whether a given directory already holds a project file
   -- (according to the GINAT Specification a project dirctory may only
   -- hold one project file).
   --
   -- Parameters:
   --   Project_Directory - A directory.
   -- Returns:
   --   True, if "Project_Directory" already holds a project file;
   --   False, otherwise.
   -- Raises:
   --   Invalid_Project_Directory_Excpetion - Raised if the passed directory
   --     "Project_Directory" is not found or could not be accessed.
   function Is_Already_A_Project_File_In_Directory
     (Project_Directory : in String)
     return Boolean;

   --------------------------------------------------------------------------
   -- Returns the data necessary to identify the Bauhaus IML Graph underlying
   -- this project.
   --
   -- Does not load a project into the main memory.
   --
   -- Note
   --   The out Parameters are not checked in any way.
   --
   -- Parameters:
   --   Project_Name - The name of a project.
   --   Project_Directory - The directory where the project is located
   --     (project directory). Only absolute paths should be passed.
   --   Bauhaus_IML_Graph_File - The file of the Bauhaus IML-Graph
   --   Bauhaus_IML_Graph_File_Checksum - A checksum that may be used
   --     to check whether "Bauhaus_IML_Graph_File" holds the
   --     correct IML Graph.
   -- Raises:
   --   Project_Does_Not_Exist_Exception - Raised if the project
   --     "Project_Name" is not found in the given directory or if the
   --     project file could not be accessed.
   --   Invalid_Project_Directory_Excpetion - Raised if the passed directory
   --     "Project_Directory" is not found or could not be accessed.
   procedure Get_Bauhaus_IML_Graph_Data
     (Project_Name           : in     Valid_Names.Standard_Name;
      Project_Directory      : in     String
      Bauhaus_IML_Graph_File :    out Ada.Strings.Unbounded.Unbounded_String;
      Bauhaus_IML_Graph_File_Checksum : out Integer);

   ---------------------------------------------------------------------------
   -- Before executing this subprgram
   -- an iml graph must have been loaded by Giant.Graph_lib.
   --
   -- Protocoll:
   --   1. Determine the IML Graph for the project
   --      => "Giant.Projects.Get_Bauhaus_IML_Graph_Data"
   --   2. Load this iml graph into memory via "Giant.Graph_Lib"
   --   3. Load the Project => "Giant.Projects.Load_Project"
   --
   -- Initializes a project by reading all the data describing a project
   -- from the project file and from the management files of the
   -- projects components.
   --
   -- All relativ paths inside an xml file describing a project are 
   -- regarded as absolute paths towards "Project_Directory".
   --
   -- Parameters:
   --   Project_Name - The name of a project.
   --   Project_Directory - The directory where the project is loacted
   --     (project directory). Only absolute paths should be passed.
   -- Returns:
   --   A pointer to a new instance of the ADT describing a project.
   -- Raises:
   --   Project_Does_Not_Exist_Exception - Raised if the project
   --     "Project_Name" is not found in the given directory.
   --   Invalid_Project_Directory_Excpetion - Raised if the passed directory
   --     "Project_Directory" is not found.
   --   Wrong_IML_Graph_Loaded_Exception - Raised if the iml graph loaded
   --     by giant.graph_lib has a checksum that differs to the checksum
   --     stored in the project.
   function Load_Project
     (Project_Name : in Valid_Names.Standard_Name;
      Project_Directory : in String)
     return Project_Access;

   ---------------------------------------------------------------------------
   -- Initializes a project.
   -- A new empty project is created.
   --
   -- If necessary the project directory is created.
   --
   -- Parameters:
   --   Project_Name - The name of a project.
   --   Project_Directory - The project directory.
   --   Bauhaus_IML_Graph_File - The File holding the IML-Graph.
   --     This parameter will not be checked for correctness.
   --   Bauhaus_IML_Graph_File_Checksum - A checksum for the
   --     Bauhaus_IML_Graph_File. This parameter will not be checked
   --     for correctness.
   -- Returns:
   --   A pointer to a new instance of the ADT describing a project.
   -- Raises:
   --   Invalid_Project_Directory_Excpetion - Raised if the passed directory
   --     "Project_Directory" is not found.
   --   Directory_Holds_Already_A_Project_File_Exception - Raised if
   --     "Project_Directory" already holds a project file.
   function Create_Empty_Project
     (Project_Name                    : in Valid_Names.Standard_Name;
      Project_Directory               : in String;
      Bauhaus_IML_Graph_File          : in String;
      Bauhaus_IML_Graph_File_Checksum : in Integer)
     return Project_Access;

   ----------------------------------------------------------------------------
   -- Deallocates a Project.
   --
   -- Note
   --   This procedure performs a DEEP DEALLOCATION
   --   all visualisation windows (including their Selections), the
   --   node annotations
   --   and all Subgraphs of the project are deallocated too.
   --   As Subraphs, Node Annotations and Visualisation_Windows
   --   and Selections are pointers you should beware of dangling pointers.
   --
   --   The Project file and all managements files are not affected
   --   by the call of this subprogramm.
   --
   -- Parameters:
   --   Project - The instance of the ADT that should be deallocated.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   procedure Deallocate_Project_Deep (Project : in out Project_Access);

   ----------------------------------------------------------------------------
   -- Writes all data describing a project into its project file and
   -- the according management files for visualisations windows,
   -- all subgraphs and node-annotations.
   --
   -- After the execution of this method the state of the project (loaded
   -- into the main memory) exactly corresponds to the state of
   -- the project files.
   --
   -- Parameters:
   --   Project - The instance of the ADT that should be written into
   --     its project files.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   procedure Store_Whole_Project (Project : in Project_Access);

   ----------------------------------------------------------------------------
   -- Writes all data describing a project into a new project file (incl.
   -- the according management files for visualisations windows,
   -- subgraphs and node-annotations).
   --
   -- After the execution of this method the state of the project (loaded
   -- into the main memory) exactly corresponds to the state of
   -- the new project files.
   --
   -- The data is written into the new project files, the old project
   -- files stay unchanged.
   --
   -- The project directory of the instance of the ADT "Project_Access"
   -- is also changed, so the call of any subprogram changing the
   -- project files only changes the new project files in the new project
   -- directory.
   --
   -- If necessarry the new project directory is created.
   --
   -- Parameters:
   --   Project - The instance of the ADT that should be written into
   --             new project files.
   --   Project_Name - The new name of the project.
   --   Project_Directory - The new project directory.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter;
   --   Directory_Holds_Already_A_Project_File_Exception -- Raised if
   --     "Project_Directory" already holds a project file.
   procedure Store_Whole_Project_As
      (Project           : in Project_Access;
       Project_Name      : in Valid_Names.Standard_Name;
       Project_Directory : in String);

   ---------------------------------------------------------------------------
   -- Returns the name of a project
   --
   -- Parameters:
   --   Project - A instance of the ADT that describes a project.
   -- Returns:
   --   The name of the project.
   -- Raises
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter;
   function Get_Project_Name
      (Project : in Project_Access)
      return Straing;


   ---------------------------------------------------------------------------
   -- Returns the project directory (a path to that directory).
   --
   -- Parameters:
   --   Project - A instance of the ADT that describes a project.
   -- Returns:
   --   The path to a project directory.
   -- Raises
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Get_Project_Directory
     (Project : in Project_Access)
     return Ada.Strings.Unbounded.Unbounded_String;


   ---------------------------------------------------------------------------
   -- A.1
   -- The Bauhaus IML Graph
   ---------------------------------------------------------------------------




   ---------------------------------------------------------------------------
   -- B
   -- Visualisation Windows
   -- and  Storrage Status Management for Visualisation Windows
   --
   -- As a project is loaded due to avoid waste of main memory
   -- the data structures for the visualisation
   -- windows are not loaded into the main momory -- this
   -- will only happen when necessary.
   --
   -- This package automatically decides when this windows have to be
   -- loaded from the project files.
   --
   -- There are three different states that a visual window could
   -- logical have
   -- inside a project (this does not effect the abstarct data type
   -- Vis_Window_Management.Visual_Window_Access itself).
   --
   -- States inside Project_Access:
   --
   -- 1. Memory_Loaded
   -- All data describing the visual window is loaded into the main memory
   --
   -- - There is no management file for the visualisation window.
   --
   -- - Heap is allocated for an Instance of
   --   Vis_Window_Management.Visual_Window_Access
   --
   --
   -- 2. Memory_Loaded_File_Linked
   -- All data describing the visual window is loaded into the main memory
   --
   -- - There is a management File for the visualisation window.
   --   The Project (Project_Access) knows that file.
   --
   -- - Heap is allocated for an Instance of
   --   Vis_Window_Management.Visual_Window_Access
   --
   --
   -- 3. File_Linked
   -- Most of the date describing the visual window is still only present
   -- in a management file.
   --
   -- - There is a management File for the visualisation window.
   --   The Project (Project_Access) knows that file.
   --
   -- - NO Heap is allocated for an instance of
   --   Vis_Window_Management.Visual_Window_Access. The project
   --   only knows the name of the visualisation window and there
   --   to find the management file.
   --
   -- Some functions require a certain state or change the state.
   -- This is described in the following manner:
   --
   -- possible startinge state            state after execution
   -- ------------------------            ---------------------
   -- File_Linked                 -->     Memory_Loaded_File_Linked
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Raised is a visualisation window is asked for that is not
   -- part of the project
   Visualisation_Window_Is_Not_Part_Of_Project     : Exception

   ---------------------------------------------------------------------------
   -- Raised if a visualisation window that should be added to the
   -- project already exists.
   Visualisation_Window_Is_Already_Part_Of_Project : Exception;

   ---------------------------------------------------------------------------
   -- Determines whether a given Visualisation Window is part of
   -- the project.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window_Name - The name (unique inside a project) of a
   --     visualisation window.
   -- Returns:
   --   True, if the project has a visualisation window with passed name
   --     "Vis_Window_Name"; False, otherwise.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Does_Visualisation_Window_Exist
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name)
     return Boolean;

   ---------------------------------------------------------------------------
   -- Returns the names of all visualisation windows of the project.
   --
   -- The returned List is sorted in ascending alphabetical order
   -- If the project has no visualisation windows a empty list will
   -- be returned.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   -- Returns:
   --   The names of all visualisation windows of the project.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Get_All_Visualisation_Window_Names_Sorted
      (Project : in Project_Access)
      return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Returns a visualisation window of a project.
   --
   -- Note
   --   The instance of the ADT describing a visualisation window
   --   ("Vis_Window_Management.Visual_Window_Access") is a pointer
   --   into the internal data structure of "Project_Access".
   --   You may change the content of a visualisation window,
   --   but UNDER NO CIRCUMSTANCES you may DEALLOCATE the returned
   --   instance before the visualisation window is removed from the
   --   project.
   --
   --   If this subprogramm is called several times with the same
   --   parameters ALIASES will be returned.
   --
   -- State Changes:
   --   Memory_Loaded_File_Linked  --> Memory_Loaded_File_Linked
   --   File_Linked                --> Memory_Loaded_File_Linked
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window_Name - The name (unique inside a project) of a
   --     visualisation window.
   -- Returns:
   --   The ADT describing a visualisation window.
   -- Raises:
   --   Visualisation_Window_Is_Not_Part_Of_Project_Exception - raised
   --     if no visualisation window with the name "Vis_Window_Name"
   --     exists.
   --  Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Get_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name)
     return Vis_Window_Management.Visual_Window_Access;

   ---------------------------------------------------------------------------
   -- Adds a visualisation window to the project.
   --
   -- Note
   --   Afterwards the datastructure describing a visualisation window
   --   ("Vis_Window_Management.Visual_Window_Access") is part of
   --   the internal datastructure of "Project_Access".
   --   You may change the content of the window, but you may NOT
   --   DEALLOCATE the visualisation window before it is removed
   --   from the project.
   --
   -- State Changes:
   --   Visualisation Window not part of project --> Memory_Loaded
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window - The visualisation window that should be added
   --   to the project.
   -- Raises:
   --   Visualisation_Window_Is_Already_Part_Of_Project_Exception - Raised
   --     there is already a visualisation window with the same name
   --     as "Vis_Window" part of the project.
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   --   Vis_Window_Management.Vis_Window_Data_Access_
   --     Not_Initialized_Exception
   --     - Raised if "Vis_Window" is not initialized.
   procedure Add_Visualisation_To_Project
    (Project    : in Project_Access;
     Vis_Window : in Vis_Window_Management.Visual_Window_Access);

   ---------------------------------------------------------------------------
   -- Writes the data of a single visualisation window into
   -- the project file for that visualisation window.
   -- That should be done before the visualisation window is closed
   -- inside the project ("procedure Close_Window_In_Project").
   --
   -- State Changes:
   --   Memory_Loaded_File_Linked  --> Memory_Loaded_File_Linked
   --   Memory_Loaded              --> Memory_Loaded_File_Linked
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window_Name - The name of a visualisation window.
   -- Raises:
   --   Visualisation_Window_Is_Not_Part_Of_Project - Raised
   --     if "Project" has no visualisation window with "Vis_Window_Name".
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   procedure Store_Single_Project_Visualisation_Window
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   -- Changes the status of a visualisation window model to "File_Linked".
   --
   -- This method does not write the data used for a visualisation
   -- window into the management file for that window.
   --
   -- Note !
   --   The heap memory needed for the visualisation window
   --   is DEALLOCATED. As Vis_Window_Management.Visual_Window_Data_Access
   --   is a Access Type, several pointers may reference the
   --   same datastructure for a visualisation window model;
   --   i.e. all instances of Visual_Window_Data_Access returned by the
   --   function call
   --
   --  >"Project_Management.Get_Visualisation_Window(Project_A, My_Window_X)"
   --
   --   will be DANGLING POINTERS after the call of
   --
   --  >"Close_Window_In_Project(Project_A, My_Window_X)"
   --
   --
   -- For a visualisation window that is not loaded into the
   -- main memory (Status: File_Linked) nothing will happen.
   --
   -- State Changes:
   --   Memory_Loaded_File_Linked  --> File_Linked
   --   Memory_Loaded              --> Visualisation Window is completely
   --                                  REMOVED
   --   File_Linked                --> File_Linked
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window_Name - The name of a visualisation window.
   --
   -- Raises:
   --   Visualisation_Window_Is_Not_Part_Of_Project - Raised
   --     if "Project" has no visualisation window with "Vis_Window_Name".
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   procedure Close_Window_In_Project
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name);


   ---------------------------------------------------------------------------
   -- Removes the visualisation window with the name "Vis_Window_Name"
   -- from the project.
   --
   -- Note
   --   The visualisation window is only removed from "Project"'s
   --   internal data structure, but NOT deallocated.
   --
   --   Take care to avoid memory leaks as the corresponding instance
   --   of Vis_Window_Management.Visual_Window_Data_Access has
   --   to be deallocated separately.
   --
   --   After the call of this subprogram you may deallocate the cooreponding
   --   instance of the visualisation window
   --   ("Vis_Window_Management.Visual_Window_Data_Access")
   --   without affecting the internal datastructure of "Project".
   --
   --
   -- If it exists the management file for this visualisation window
   -- is DELETED too.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Vis_Window_Name - The name of a visualisation window.
   --
   -- Raises:
   --   Visualisation_Window_Is_Not_Part_Of_Project_Exception - raised
   --     if no visualisation window with the name "Vis_Window_Name"
   --     exists.
   --  Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   procedure Remove_Vis_Window_From_Project;
     (Project         : in Project_Access;
      Vis_Window_Name : in Valid_Names.Standard_Name);


   ---------------------------------------------------------------------------
   -- C Subgraphs
   --
   -- All functionality necessarry to administrate Subgraphs
   -- inside a project is specified here.
   -- Needed to handle the IML-Subgraphs 
   --
   -- -> see GIANT Spec. "12.2.3. Persistenz von IML-Teilgraphen"
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This Exception is raised if a demanded Subgraph does not
   -- exist in the project
   Subgraph_Is_Not_Part_Of_Project_Exception     : Exception

   ---------------------------------------------------------------------------
   -- Raised if an Subgraph that should be added to the
   -- project already exists.
   Subgraph_Is_Already_Part_Of_Project_Exception : Exception;

   ---------------------------------------------------------------------------
   -- Determines whether a subgraph with the given name exists.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Subgraph_Name - The name of a subgraph.
   -- Returns:
   --   True, if the subgraph exists; False, otherwise.
   -- Raises:
   --  Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Does_Subgraph_Exist
     (Project           : in Project_Access;
      Subgraph_Name : in Valid_Names.Standard_Name)
     return Boolean;

   ---------------------------------------------------------------------------
   -- Returns a subgraph from the project.
   --
   -- Note !!!
   --   The returned ATD is a pointer into the internal data structure
   --   of "Project".
   --   You man NOT DEALLOCATE the returned subgraph
   --   before it is removed from the Project.
   --
   --   Several calls of this function with the same parameters will
   --   cause ALIASES.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Subgraph_Name - The name of a subgraph.
   -- Returns:
   --    The ADT describing a subgraph.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   --   Subgraph_Is_Not_Part_Of_Project_Exception - Raised if
   --   "Project" has no subgraph with the name "Subgraph_Name".
   function Get_Subgraph
     (Project           : in Project_Access;
      Subgraph_Name : in Valid_Names.Standard_Name)
     return Graph_Lib.IML_Subgraphs.IML_Subgraph_Access;

   ---------------------------------------------------------------------------
   -- Returns the names of all subgraphs of the project (not sorted by 
   -- any order).
   -- If there are no subgraphs part of the project than
   -- an empty list will be returned.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   -- Returns
   --   A list containing the names of all subgraphs of this project.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   function Get_All_Sungraphs
     (Project : in Project_Access)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Adds a subgraph to a project.
   --
   -- Note
   --   The instance of the ADT "Graph_Lib.IML_Subgraphs.IML_Subgraph_Access"
   --   becomes part of the internal data structure of "Project".
   --   You are NOT ALLOWED to change the name of the subgraph
   --   or to DEALLOCATE it as long as it is part of the project
   --   (before doing so you have to remove the subgraph from the
   --    project).
   --
   --   Each subgraph in the project must have an unique name.
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Subgraph - The new subgraph.
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   --   Subgraph_Is_Already_Part_Of_Project_Exception - Raised if
   --     the project already has a subgraph with the same name
   --     as "Subgraph".
   procedure Add_Subgraph
     (Project      : in Project_Access;
      Subgraph : in Graph_Lib.IML_Subgraphs.IML_Subgraph_Access);

   ---------------------------------------------------------------------------
   -- Removes a subgraph from the project.
   --
   -- Note!
   --   The subgraph is only removed from the project
   --   no deep deallocation is done (the corresponding instance
   --   of Graph_Lib.IML_Subgraphs.IML_Subgraph_Access is not
   --   deallocated).
   --   Beware of memory leacks!
   --
   --   After the call of that subprogram you may do what ever you want
   --   with the removed subgraph without affecting the internal
   --   datastructure of "Project".
   --
   -- Parameters:
   --   Project - The instance of the ADT holding a project.
   --   Subgraph_Name - The name of the subgraph that should be
   --     removed from the "Project".
   -- Raises:
   --   Project_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Project_Access" is passed as
   --     parameter.
   --   Subgraph_Is_Not_Part_Of_Project_Exception - Raised if the
   --     project "Project" does not hold a subgraph with the name
   --     "IML_Subgraph_Name".
   procedure Remove_Subgraph
      (Project           : in Project_Access;
       Subgraph_Name : in Valid_Names.Standard_Name);

   ---------------------------------------------------------------------------
   -- D Node Annotations
   -- This part specifies the functionality needed to get access to the
   -- node annotations of a project.
   --
   -- The functionality to manipulate the annoations has been removed
   -- from this package in order to avoid a cyclic package dependency
   -- it is now offered by the package "Node_Annotation_Management".
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Returns the instance of the ADT "Node_Annotation_Access" that holds
   -- all node annotations of this package.
   --
   -- Note:
   --   The returned instance is a pointer into the internal data model
   --   of "Project_Access" (no deep copy).
   --   Every changes you perform by the functionality of the package
   --   "Node_Annotation_Management" directly change the internal
   --   data structure.
   --
   --   UNDER NO CIRCUMSTANCES you may DEALLOCATE the returned pointer.
   --
   --   Several calls of this subprogram with the same parameters will
   --   cause aliases.
   --
   --   After the deallocation of "Project_Access" the returned pointers
   --   are dangling pointers.
   --
   -- Parameters:
   --   Project - The project whose annotations should be returned.
   -- Returns:
   --   An instance of the ADT holding all node annotations of this
   --   project.
   -- Raises
   !!!!!!!!!!!!!
   function Get_Node_Annotations_Of_Project
     (Project : in Project_Access)
     return Node_Annotation_Management.Node_Annotation_Access;


------------------------------------------------------------------------------
private

   ---------------------------------------------------------------------------
   -- Management of visualisation window data models
   ---------------------------------------------------------------------------

   -- needed to describe the status of a visualistion window
   Vis_Window_Data_Element is record 
      Is_File_Linked           : Boolean;
      Is_Memory_Loaded         : Boolean;
      -- the management file for the visualisation window
      -- null string ("") if such a file does not exist yet
      Existing_Vis_Window_File : Ada.Strings.Unbounded.Unbounded_String;
      Vis_Window_Name          : Ada.Strings.Unbounded.Unbounded_String;      
   end record;

   package Known_Vis_Windows_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Vis_Window_Data_Element); 

   package Memory_Loaded_Vis_Window_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Vis_Windows.Visual_Window_Access);
      
   ---------------------------------------------------------------------------
   -- Management of Subgraphs
   ---------------------------------------------------------------------------

   type Subgraph_Data_Elemet is record
     Subgraph         : Graph_Lib.Subgraphs.Subgraph;
     Highlight_Status : Subgraph_Highlight_Status;
     -- True if an management file for the iml subgraph already exists, False     
     -- otherwise.
     Is_File_Linked   : Boolean:
     -- null string ("") if such a file does not exist yet
     Existing_Subgraph_File    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Subgraph_Data_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Subgraph_Data_Elemet);

  ----------------------------------------------------------------------------
  -- Project Data object
  ----------------------------------------------------------------------------

  -- The dataobject describing a project
  type Project_Element;

  -- The Pointer to that dataobject
  type Project_Access is access Project_Element;

  type Project_Element is record

    -- The name of the project
    Project_Name     : Ada.Strings.Unbounded.Unbounded_String;
    
    -- The directory there all data (including management files
    -- for visualisation windows, Subgraphs and the
    -- management file for node annatations) describing the
    -- whole project has to be located.
    Project_Dirctory : Ada.Strings.Unbounded.Unbounded_String;

    -- The file holding the Bauhaus IML-Graph
    Bauhaus_IML_Graph_File : Ada.Strings.Unbounded.Unbounded_String;
    
    -- Checksum of the Bauhaus IML Graph
    Bauhaus_IML_Graph_File_Checksum : Integer;
    
    -- The file holding the node annotations
    Node_Annotations_File : Ada.Strings.Unbounded.Unbounded_String;

    -- All Vis Windows that are part of the project.
    All_Project_Vis_Windows : Known_Vis_Windows_Hashs.Mapping;

    -- Only Visualisation Windows that a loaded into the main
    -- memory -> Heap is allocated for an Instance of
    -- Visual_Window_Access.
    -- A Management File in the project dirctory may exist or not
    All_Memory_Loaded_Vis_Windows : Memory_Loaded_Vis_Window_Hashs.Mapping;

    All_Subgraphs : Subgraph_Data_Hashs.Mapping;

    -- The node annotations
    The_Node_Annotations : Node_Annotations.Node_Annotation_Access;

  end record;

end Giant.Projects;


