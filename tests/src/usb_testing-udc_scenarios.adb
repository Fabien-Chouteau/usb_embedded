------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Assertions;
with USB.Device; use USB.Device;
with USB_Testing.Class_Stub;
with USB_Testing.Output;

package body USB_Testing.UDC_Scenarios is

   --------------------
   -- Basic_UDC_Test --
   --------------------

   procedure Basic_UDC_Test (Scenario      : aliased UDC_Stub.Stub_Scenario;
                             Expected      :         AAA.Strings.Vector;
                             RX_Data       : aliased UInt8_Array;
                             Early_Address : Boolean := False;
                             Init_Verbose  : Boolean := False)
   is
      Output : aliased USB_Testing.Output.Text_Output;

      Class : aliased Class_Stub.Device_Class_Stub (Output'Unchecked_Access,
                                                    1);

      UDC : aliased UDC_Stub.Controller (Output'Unchecked_Access,
                                         Scenario'Unchecked_Access,
                                         RX_Data'Unchecked_Access,
                                         Has_Early_Address => Early_Address,
                                         Max_Packet_Size   => 64,
                                         EP_Buffers_Size   => 256,
                                         Number_Of_EPs     => 10,
                                         Init_Verbose      => Init_Verbose);
      Ctrl   : USB.Device.USB_Device_Stack (Max_Classes => 1);
      Result : USB.Device.Init_Result;
   begin

      AUnit.Assertions.Assert
        (Ctrl.Register_Class (Class'Unchecked_Access),
         "USB STACK register class failed");

      Result := Ctrl.Initialize
        (Controller      => UDC'Unchecked_Access,
         Manufacturer    => To_USB_String ("Manufacturer"),
         Product         => To_USB_String ("Product"),
         Serial_Number   => To_USB_String ("Serial"),
         Max_Packet_Size => 64);

      AUnit.Assertions.Assert
        (Result = Ok, "USB STACK Init failed: " & Result'Img);

      Ctrl.Start;

      loop
         Ctrl.Poll;
         pragma Warnings (Off, "possible infinite loop");
         exit when UDC.End_Of_Scenario;
         pragma Warnings (On, "possible infinite loop");
      end loop;

      declare
         use USB_Testing.Output;

         Actual : constant AAA.Strings.Vector := Output.Dump;
      begin
         AUnit.Assertions.Assert
           (Equal (Expected, Actual),
            "Diff in output: " & ASCII.LF &
              Diff (Expected, Actual, "Expected", "Actual").Flatten (ASCII.LF));

      end;
   end Basic_UDC_Test;

   --------------------------
   -- Two_Classes_UDC_Test --
   --------------------------

   procedure Two_Classes_UDC_Test (Scenario     : aliased UDC_Stub.Stub_Scenario;
                                   Expected     :         AAA.Strings.Vector;
                                   RX_Data      : aliased UInt8_Array;
                                   Init_Verbose : Boolean := False)
   is
      Output : aliased USB_Testing.Output.Text_Output;

      Class1 : aliased Class_Stub.Device_Class_Stub (Output'Unchecked_Access,
                                                     1);

      Class2 : aliased Class_Stub.Device_Class_Stub (Output'Unchecked_Access,
                                                     2);

      UDC : aliased UDC_Stub.Controller (Output'Unchecked_Access,
                                         Scenario'Unchecked_Access,
                                         RX_Data'Unchecked_Access,
                                         Has_Early_Address => False,
                                         Max_Packet_Size   => 64,
                                         EP_Buffers_Size   => 256,
                                         Number_Of_EPs     => 10,
                                         Init_Verbose      => Init_Verbose);

      Ctrl : USB.Device.USB_Device_Stack (Max_Classes => 2);
      Result : USB.Device.Init_Result;
   begin

      AUnit.Assertions.Assert
        (Ctrl.Register_Class (Class1'Unchecked_Access),
         "USB STACK register class1 failed");

      AUnit.Assertions.Assert
        (Ctrl.Register_Class (Class2'Unchecked_Access),
         "USB STACK register class2 failed");

      Result := Ctrl.Initialize
        (Controller      => UDC'Unchecked_Access,
         Manufacturer    => To_USB_String ("Manufacturer"),
         Product         => To_USB_String ("Product"),
         Serial_Number   => To_USB_String ("Serial"),
         Max_Packet_Size => 64);

      AUnit.Assertions.Assert
        (Result = Ok, "USB STACK Init failed: " & Result'Img);

      Ctrl.Start;

      loop
         Ctrl.Poll;
         pragma Warnings (Off, "possible infinite loop");
         exit when UDC.End_Of_Scenario;
         pragma Warnings (On, "possible infinite loop");
      end loop;

      declare
         use USB_Testing.Output;

         Actual : constant AAA.Strings.Vector := Output.Dump;
      begin
         AUnit.Assertions.Assert
           (Equal (Expected, Actual),
            "Diff in output: " & ASCII.LF &
              Diff (Expected, Actual, "Expected", "Actual").Flatten (ASCII.LF));

      end;

   end Two_Classes_UDC_Test;

end USB_Testing.UDC_Scenarios;
