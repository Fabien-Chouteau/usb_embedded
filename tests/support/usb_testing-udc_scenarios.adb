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

with USB;

with USB_Testing.Class_Stub;

package body USB_Testing.UDC_Scenarios is

   --------------------
   -- Basic_UDC_Test --
   --------------------

   procedure Basic_UDC_Test (Scenario      : aliased UDC_Stub.Stub_Scenario;
                             RX_Data       : aliased UInt8_Array;
                             Early_Address : Boolean := False)
   is
      Configuration : aliased constant UInt8_Array := (0 .. 1 => 0);

      Class : aliased Class_Stub.Device_Class_Stub;

      UDC : aliased UDC_Stub.Controller (Scenario'Unchecked_Access,
                                         RX_Data'Unchecked_Access,
                                         Has_Early_Address => Early_Address);
      Ctrl : USB.USB_Device;
   begin

      Ctrl.Initalize (Controller => UDC'Unchecked_Access,
                      Class      => Class'Unchecked_Access,
                      Dec        => USB_Testing.UDC_Stub.Desc'Unchecked_Access,
                      Config     => Configuration'Unchecked_Access,
                      Strings    => USB_Testing.UDC_Stub.Strings'Unchecked_Access);

      Ctrl.Start;

      loop
         Ctrl.Poll;
         pragma Warnings (Off, "possible infinite loop");
         exit when UDC.End_Of_Scenario;
         pragma Warnings (On, "possible infinite loop");
      end loop;
   end Basic_UDC_Test;

end USB_Testing.UDC_Scenarios;
