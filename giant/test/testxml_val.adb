with Input_Sources.File;
with Tree_Readers;
with Sax.Readers;

with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Ada.Exceptions; use  Ada.Exceptions;
with Ada.Text_IO;

procedure Testxml_Val is

   ---------------------------------------------------------------------------
   -- Exceptions thrown by xmlada are not cached.
   --
   -- The validation done by xmlada does not detect all
   -- possible faults. The author ("Martin Schwienbacher")
   -- supposes that the parser will possible also accept
   -- non valid xml files even though the "validation
   -- feature" is activated.
   --
   -- The in out Parameter "Tree_Reader" is needed for deallocation
   -- after processing the xml file.
   procedure Load_XML_File_Validated
     (File         : in String;
      Tree_Reader  : in out Tree_Readers.Tree_Reader;
      XML_Document : out Dom.Core.Document) is

      -- The input file
      Input_File   : Input_Sources.File.File_Input;

   begin

      -- open input file
      Input_Sources.File.Open (File, Input_File);

      -- activate "validation feature"
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Validation_Feature, True);

      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Namespace_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.External_General_Entities_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.External_Parameter_Entities_Feature, True);
      Tree_Readers.Set_Feature
        (Tree_Reader, Sax.Readers.Parameter_Entities_Feature, True);

      Ada.Text_IO.Put_Line(">>>>>>>>>>>>>>>>>Start_Parsing");
      -- read tree from file - completely into main memory
      Tree_Readers.Parse (Tree_Reader, Input_File);
      Ada.Text_IO.Put_Line(">>>>>>>>>>>>>>>>>Finish_Parsing");

      -- close input file
      Input_Sources.File.Close (Input_File);

      -- The first element of the document the "Document-Element"
      XML_Document := Tree_Readers.Get_Tree (Tree_Reader);

   end Load_XML_File_Validated;

   ---------------------------------------------------------------------------
   -- As xmlada seems not to be able to retrieve the DTD associated
   -- to a parsed document (type "Dom.Core.Document") GIANT differs
   -- separate types of xml files (e.g. for visualisation styles and
   -- global config data) by the tag name of the top level node
   -- (the "document element").
   --
   -- Returns :
   --   True if "Type_Name" is equal to the tag name of the
   --   "document element"
   function Does_XML_Document_Belong_To_Type
     (Type_Name           : in String;
      Parsed_XML_Document : in Dom.Core.Document) return Boolean is

      Top_Level_Element : DOM.Core.Element;

   begin

      Top_Level_Element :=
        DOM.Core.Documents.Get_Element(Parsed_XML_Document);

      if (Type_Name = DOM.Core.Nodes.Node_Name(Top_Level_Element)) then
         return True;
      end if;

      return False;
   end Does_XML_Document_Belong_To_Type;


   ------
   The_Tree_Reader  : Tree_Readers.Tree_Reader;
   The_XML_Document : Dom.Core.Document;

--   A_Node : DOM.Core.Node;
   Nodes_List : DOM.Core.Node_List;

begin

   -- Load xml tree
   Load_XML_File_Validated
     ("../src/config/giant_vis_style.xml",
      The_Tree_Reader,
      The_XML_Document);

   -- check file type
   if ( Does_XML_Document_Belong_To_Type
          ("giant_visualisation_style_file",
           The_XML_Document)
        = False ) then

      Ada.Text_IO.Put_Line("Wrong type of File");
   end if;


   -- Access File Content
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("----------------------------------");
   Ada.Text_IO.Put_Line("File loeaded successful");

   DOM.Core.Nodes.Print (The_XML_Document,
          Print_Comments => True,
          Print_XML_PI => True,
          With_URI => True);

exception
   when E : Sax.Readers.XML_Fatal_Error =>
            -- Input_Sources.Close (Read);
            Ada.Text_IO.Put_Line (Exception_Message (E));
            Tree_Readers.Free (The_Tree_Reader);
end Testxml_Val;
