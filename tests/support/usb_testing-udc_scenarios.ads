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

with AAA.Strings;
with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;

with HAL; use HAL;
with USB; use USB;
with USB.HAL.Device; use USB.HAL.Device;

package USB_Testing.UDC_Scenarios is

   procedure Basic_UDC_Test (Scenario      : aliased UDC_Stub.Stub_Scenario;
                             Expected      :         AAA.Strings.Vector;
                             RX_Data       : aliased UInt8_Array;
                             Early_Address : Boolean := False;
                             Init_Verbose  : Boolean := False);
   --  Create a Basic UDC test setup (stub UDC and stub class) and run the test

   procedure Two_Classes_UDC_Test (Scenario     : aliased UDC_Stub.Stub_Scenario;
                                   Expected     :         AAA.Strings.Vector;
                                   RX_Data      : aliased UInt8_Array;
                                   Init_Verbose : Boolean := False);
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

        --  ACK the IN transfer with a ZLP
        , (Kind => UDC_Event_E,
           Evt  => (Kind => Transfer_Complete,
                    EP   => (0, EP_Out),
                    BCNT => 0))
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
       )
      );

   function Get_Status (Verbose : Boolean)
                        return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       Req    => ((Dev, 0, Stand, Device_To_Host),
                                  0, 0, 0, 2),
                       Req_EP => 0))
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

        --  ACK the IN transfer with a ZLP
        , (Kind => UDC_Event_E,
           Evt  => (Kind => Transfer_Complete,
                    EP   => (0, EP_Out),
                    BCNT => 0))
       )
      );

   function Set_Config (Verbose   : Boolean;
                        Config_Id : UInt16 := 1)
                        return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       Req    => ((Dev, 0, Stand, Host_To_Device),
                                  9, Config_Id, 0, 0),
                       Req_EP => 0))
       )
      );

   function Get_String (Verbose : Boolean; Id : String_Id; Ack : Boolean)
                        return UDC_Stub.Stub_Scenario
   is (((Kind => Set_Verbose, Verbose => Verbose)

        , (Kind    => UDC_Event_E,
           Evt     => (Kind   => Setup_Request,
                       Req    => ((Dev, 0, Stand, Device_To_Host),
                                  6, 16#0300# + UInt16 (Id), 0, 255),
                       Req_EP => 0))

        --  ACK the IN transfer with a ZLP
        , (if Ack then (Kind => UDC_Event_E,
                        Evt  => (Kind => Transfer_Complete,
                                 EP   => (0, EP_Out),
                                 BCNT => 0))
                  else (Kind => UDC_Event_E,
                        Evt  => (Kind    => None)))
       )
      );

end USB_Testing.UDC_Scenarios;
