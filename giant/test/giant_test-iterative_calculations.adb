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
--  $RCSfile: giant_test-iterative_calculations.adb,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Giant_Test.Concurrent_Calculations;

package body Giant_Test.Iterative_Calculations is

   function Create
     (Calculation_Number : in Natural;
      Number_Of_Steps    : in Natural)
      return Counter_Access is

      Pointer : Counter_Access;
   begin
      Pointer := new Counter;
      Giant.Evolutions.Initialize
        (Giant.Evolutions.Evolution_Class_Access (Pointer),
         Number_Of_Steps);
      Pointer.Number := Calculation_Number;
      return Pointer;
   end Create;

   procedure Step
     (Individual  : access Counter;
      Next_Action :    out Giant.Evolutions.Evolution_Action) is

      J : Integer := 0;
   begin
      for I in 1 .. 1_000_000 loop
         J := (J + I) mod 17;
      end loop;
      if J > 0 then
         null;
      else
         null;
      end if;
      Giant.Evolutions.Advance_Progress (Individual, 1);
      if Giant.Evolutions.Get_Progress_Count (Individual) >=
        Giant.Evolutions.Get_Complexity (Individual) then

         Next_Action := Giant.Evolutions.Finish;
      else
         if Giant.Evolutions.Get_Progress_Count (Individual)
           mod (Giant.Evolutions.Get_Complexity (Individual) / 2 + 10) = 0 then

            Next_Action := Giant.Evolutions.Synchronize;
         elsif Giant.Evolutions.Get_Progress_Count (Individual)
           mod (Giant.Evolutions.Get_Complexity (Individual) / 3 + 10) = 0 then

            Giant.Evolutions.Start_Sub_Calculation
              (Individual, Concurrent_Calculations.Create (123, 100));

            Next_Action := Giant.Evolutions.Run;

         else

            Next_Action := Giant.Evolutions.Run;
         end if;
      end if;
   end Step;

   procedure Synchronized_Step
     (Individual   : access Counter;
      Next_Action  :    out Giant.Evolutions.Evolution_Action) is
   begin
      Put_Line ("Now doing synchronized action.");
      Next_Action := Giant.Evolutions.Run;
   end Synchronized_Step;

   procedure Finish
     (Individual : access Counter;
      Canceled   : in     Boolean) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Counter, Counter_Access);

      Local_Copy : Counter_Access;
   begin
      Put ("Action" & Natural'Image (Individual.Number));
      if Canceled then
         Put_Line (" was canceled.");
      else
         Put_Line (" has finished.");
      end if;

      Local_Copy := Counter_Access (Individual);
      Free (Local_Copy);
   end Finish;

end Giant_Test.Iterative_Calculations;
