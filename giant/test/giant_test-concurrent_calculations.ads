------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und ‹bersetzerbau, University of Stuttgart for
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
--  $RCSfile: giant_test-concurrent_calculations.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:58 $
--
------------------------------------------------------------------------------


with Giant.Evolutions;

package Giant_Test.Concurrent_Calculations is

   type Counter is new Giant.Evolutions.Concurrent_Evolution with private;

   type Counter_Access is access all Counter;
   type Counter_Class_Access is access all Counter'Class;

   function Create
     (Calculation_Number : in Natural;
      Number_Of_Steps    : in Natural)
      return Counter_Access;

   procedure Step
     (Individual  : access Counter;
      Next_Action :    out Giant.Evolutions.Evolution_Action);

   procedure Synchronized_Step
     (Individual   : access Counter;
      Next_Action  :    out Giant.Evolutions.Evolution_Action);

   procedure Finish
     (Individual : access Counter;
      Canceled   : in     Boolean);

private

   type Counter is new Giant.Evolutions.Concurrent_Evolution with
      record
         Number    : Natural;
      end record;

end Giant_Test.Concurrent_Calculations;
