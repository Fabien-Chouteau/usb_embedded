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

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;

with HAL; use HAL;
with USB; use USB;
with USB.Device;     use USB.Device;
with USB.HAL.Device; use USB.HAL.Device;


package USB_Testing.UDC_Scenarios is

   procedure Basic_UDC_Test (Scenario      : aliased UDC_Stub.Stub_Scenario;
                             RX_Data       : aliased UInt8_Array;
                             Early_Address : Boolean := False);
   --  Create a Basic UDC test setup (stub UDC and stub class) and run the test

   procedure Two_Classes_UDC_Test (Scenario : aliased UDC_Stub.Stub_Scenario;
                                   RX_Data  : aliased UInt8_Array);
   --  Create an UDC test setup with two classses and run the test

   -- Commonly used pieces of UDC scenario --

   function Enumeration (Verbose : Boolean)
                         return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind => Reset))

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       --  Get Device descriptor
                       Req    => ((Dev, 0, Stand, Device_To_Host),
                                  6, 16#0100#, 0, 64),
                       Req_EP => 0))
       )
      );

   function Set_Address (Verbose : Boolean;
                         Addr    : UInt16 := 42)
                         return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       Req    => ((Dev, 0, Stand, Host_To_Device),
                                  5, Addr, 0, 0),
                       Req_EP => 0))
        , (Kind    => Transfer_All,
           EP      => (0, EP_In))
       )
      );

   function Get_Config (Verbose : Boolean)
                        return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       Req    => ((Dev, 0, Stand, Device_To_Host),
                                  6, 16#0200#, 0, 255),
                       Req_EP => 0))

        , (Kind    => Transfer_All,
           EP      => (0, EP_In))
       )
      );

end USB_Testing.UDC_Scenarios;
