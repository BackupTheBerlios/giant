------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-edge_class_proc.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:53 $
--
--  -----
--  This package others functionality to process edge class entries in
--  xml files that may hold vildcards ("*").
--
with Giant.Graph_Lib; -- from GIANT

with DOM.Core.Nodes;  -- from xmlada

package Giant.Edge_Class_Proc is

   ---------------------------------------------------------------------------
   --  This function returns all edge classes a xml edge class entry 
   --  that may hold vildcards stands for.
   --
   --  Note  the node describing the edge class must have the follwing
   --  attributes:
   --
   --  <!ATTLIST edge_class
   --    start_node_class CDATA #REQUIRED
   --    attribute_name   CDATA #REQUIRED
   --  >
   --
   --  The following values are allowed:
   --  
   --  1. start_node_class = "node_class" attribute_name = "attribute_name"
   --  -> Will return exactly one edge class
   --  
   --  2. start_node_class = "*" attribute_name = "attribute_name"
   --  -> Will return all edge classes described by the given
   --     attribute_name, regardeless of the node class.
   --
   --  3. start_node_class = "node_class" attribute_name = "*"
   --  -> Will return all edge classes that belong to the node class
   --     "node_class".
   --  
   --  Parameters:
   --    XML_Node - A XML Node that describes an edge class.
   --  Returns:
   --    A set holding all appropriate edge classes, if none are found an
   --    empty set will be returned.
   function Process_Edge_Class_Entry 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Edge_Class_Id_Set;

end Giant.Edge_Class_Proc;
