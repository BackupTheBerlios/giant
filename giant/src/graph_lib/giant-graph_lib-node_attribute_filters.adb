package body Giant.Graph_Lib.Node_Attribute_Filters is

   ------------
   -- Create --
   ------------

   function Create
     (Node_Class          : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
      return Filter
   is
   begin
      return Create (Node_Class, Node_Attribute_Names_List);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Node_Attribute_Filter  : in out Filter)
   is
   begin
      null;
   end Destroy;

   ------------------------------
   -- Iterator_Current_Element --
   ------------------------------

   procedure Iterator_Current_Element
     (Node_Attribute_Filter   : in     Filter;
      Node                    : in     Node_Id;
      Attribute               :    out Node_Attribute_Id)
   is
   begin
      null;
   end Iterator_Current_Element;

   ----------------------
   -- Iterator_HasMore --
   ----------------------

   function Iterator_HasMore
     (Node_Attribute_Filter   : in     Filter;
      Node                    : in     Node_Id)
      return Boolean
   is
   begin
      return Iterator_HasMore (Node_Attribute_Filter, Node);
   end Iterator_HasMore;

   --------------------
   -- Iterator_Reset --
   --------------------

   procedure Iterator_Reset
     (Node_Attribute_Filter   : in     Filter;
      Node                    : in     Node_Id)
   is
   begin
      null;
   end Iterator_Reset;

end Giant.Graph_Lib.Node_Attribute_Filters;

