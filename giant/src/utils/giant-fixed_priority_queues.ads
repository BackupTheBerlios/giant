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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-fixed_priority_queues.ads,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------
--
--  Generic heap implementation of a priority queue with a fixed maximum size.
--
--  The type of items is a generic parameter. A strict total order must
--  be provided on that type ("priority order").
--
--  Two subprograms must be provided as generic parameters that allow to
--  associate a Natural value with an item. Each item can be contained
--  in one queue at most. These subprograms are used to perform membership
--  and update operations in O (1)
--
--  If memebership tests and update of priorities for item other than
--  the head item are not needed then the package
--  Giant.Simple_Priority_Queues should be used.
--


generic

   ----------------------------------------------------------------------------
   --  Type for items
   type Item_Type is private;

   ----------------------------------------------------------------------------
   --  Priority order. This function must either always return the
   --  same value for the same arguments, or the data structure must
   --  be updated by calls to 'Update_Item' or 'Update_Head'
   --
   --  Returns:
   --    True if 'Left' has a higher priority than 'Right', False else
   with function Has_Higher_Priority
     (Left  : in Item_Type;
      Right : in Item_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Associates the value of 'Position' with the item 'Item'. Should not
   --  be called from outside of this package.
   --
   --  Postcondition:
   --    Get_Position (Item) = Position
   with procedure Set_Position
     (Item     : in     Item_Type;
      Position : in     Natural);

   ----------------------------------------------------------------------------
   --  Returns the value previously associates with 'Item' by a call
   --  to 'Set_Position'.
   --
   --  Returns:
   --    P if preceeded by a call 'Set_Position (Item, P)',
   --    0 if 'Set_Position' has never been called on 'Item'
   with function Get_Position
     (Item : in     Item_Type)
     return Natural;

package Giant.Fixed_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Type for fixed size priority queues of size at most 'Max_Size' elements
   type Queue_Type (Max_Size : Natural) is private;

   ----------------------------------------------------------------------------
   --  Raised whenever the size of a  Q : 'Queue_Type' exceeds
   --  'Get_Max_Size (Q)' items
   Too_Many_Items   : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a 'Queue_Type' is empty, but an item is needed
   Not_Enough_Items : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a method expects one special item in a 'Queue_Type',
   --  but that item is not contained in that 'Queue_Type'
   Unknown_Item     : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever an item is inserted into a queue but is already
   --  contained in one
   Invalid_Item     : exception;

   ----------------------------------------------------------------------------
   --  Inserts an item into a queue. Note that each item must be
   --  contained at most in one queue at any point of time.
   --
   --  Precondition:
   --    Get_Size (Queue) < Get_Max_Size (Queue) and
   --    Get_Position (Item) = 0
   --    for all Q : Queue_Type: not Contains (Q, Item)
   --  Postcondition:
   --    Contains (Queue, Item)
   --    and not Is_Empty (Queue)
   --    and Get_Position (Item) /= 0
   --  Raises:
   --    * Too_Many_Items if Get_Size (Queue) >= Get_Max_Size (Queue)
   --    * Invalid_Item if Item already contained in some queue
   --                   precisely: Get_Position (Item) /= 0
   --  Warning:
   --    Set_Position (Item) must never be called from outside this package
   --    until 'Item' is removed from 'Queue'
   procedure Insert
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Must be called whenever the priority of the head item has changed.
   --
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Update_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Must be called whenever the priority of 'Item' has changed.
   --
   --  Precondition:
   --    Contains (Queue, Item)
   --  Raises:
   --    Unknown_Item if Precondition not satisfied
   procedure Update_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Removes the head item from 'Queue'
   --
   --  Precondition:
   --    not Is_Empty (Queue) and I := Get_Head (Queue)
   --  Postcondition:
   --    not Contains (Queue, I) and
   --    Get_Position (I) = 0
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Remove_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Precondition:
   --    Contains (Queue, Item)
   --  Postcondition
   --    not Contains (Queue, Item) and
   --    Get_Position (Item) = 0
   --  Raises:
   --    Unknown_Item if Precondition not satisfied
   procedure Remove_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Precondition:
   --    True and Former_Content := {I | Contains (Queue, I)}
   --  Postcondition:
   --    Is_Empty (Queue)
   --    for all I in Former_Content: Get_Position (I) = 0
   procedure Clear
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Returns an item contained in 'Queue' that has the maximum priority.
   --  If there are more than one items in 'Queue' that have the maximum
   --  priority then it is unspecified which one will be returned. However
   --  two calls to 'Get_Head' always yield the same item unless 'Queue'
   --  was modified.
   --
   --  Returns:
   --    Head item in 'Queue':
   --    for all I : Item_Type:
   --      Contains (Queue, I) ==> not Has_Higher_Priority (I, Head)
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   function Get_Head
     (Queue : in     Queue_Type)
     return Item_Type;

   ----------------------------------------------------------------------------
   --  Checks if 'Item' is contained in 'Queue'
   --
   --  Returns:
   --    True if the statement
   --    'while Get_Head (Queue) /= Item loop Remove_Head (Queue) end loop;'
   --    does not raise an exception, False otherwise
   function Contains
     (Queue : in     Queue_Type;
      Item  : in     Item_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    for all I : Item_Type: not Contains (Queue, I)
   function Is_Empty
     (Queue : in     Queue_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    Cardinality of the set {I | Contains (Queue, I)}
   function Get_Size
     (Queue : in     Queue_Type)
     return Natural;

   ----------------------------------------------------------------------------
   --  Returns:
   --    The number of Items that can be inserted at most into 'Queue'
   function Get_Max_Size
     (Queue : in     Queue_Type)
     return Natural;

private

   subtype Array_Bounds is Positive;

   type Heap_Array is array (Array_Bounds range <>) of Item_Type;

   type Queue_Type (Max_Size : Natural) is
      record
         Size  : Natural := 0;
         Field : Heap_Array (1 .. Max_Size);
      end record;

end Giant.Fixed_Priority_Queues;
