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
--  $RCSfile: giant-iml_class_inheritance_proc.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:53 $
--
--  -----
--  This package others functionality needed to process node and 
--  edge class definitions that are based on an inheritance hierarchy.
--
with Giant.Graph_Lib; -- from GIANT

with DOM.Core.Nodes;  -- from xmlada

package Giant.IML_Class_Inheritance_Proc is

   ---------------------------------------------------------------------------
   --  Returns all node classes that inherit from a given one (incl.
   --  the Upper_Class itself).
   --
   --
   --  Note  the xml node describing the node class must have the follwing
   --  attributes:
   --
   --  <!ATTLIST super_node_class
   --    super_node_class_name CDATA #REQUIRED
   --  >  
   --
   --  Parameters:
   --    XML_Node - A XML Node that describes a node class
   --  Returns:
   --    A set holding the node class described by the passed xml node
   function Get_All_Sub_Node_Classes 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Node_Class_Id_Set;
             
   ---------------------------------------------------------------------------
   --  Retrun all edge classes that belong to the passed node class or
   --  to one of the node classes 
   --
   --
   --  <!ATTLIST super_edge_class
   --    super_start_node_class CDATA #REQUIRED
   --    attribute_name         CDATA #REQUIRED
   --  >
   --
   --  Allowed Values
   --    1. super_start_node_class = "node_class_name" 
   --       attribute_name         = "attribute_name"
   --
   --    2. super_start_node_class = "node_class_name" 
   --       attribute_name         = "*"
   --
   -- Returns a set holding all Edge_Classes with the "attribute_name"
   -- that belong to the node class "super_start_node_class" or one
   -- of its sublacces. If attribute_name ="*" then all edge classes
   -- that belong to the node class or one of its subclasses will
   -- be returned.         
   function Get_All_Sub_Edge_Classes 
     (XML_Node : in DOM.Core.Node) 
     return Graph_Lib.Edge_Class_Id_Set;

end Giant.IML_Class_Inheritance_Proc;
