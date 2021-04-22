
--  Some UDC will skip the Status_Out ZLP if a new setup request is received
--  quickly. This test checks that the Device Stack can handle it.

with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;
with USB.HAL.Device; use USB.HAL.Device;

with USB;            use USB;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => False),
                    2 => (Kind    => UDC_Event_E,
                          Evt     => (Kind   => Setup_Request,
                                      --  Get Device descriptor
                                      Req    => ((Dev, 0, Stand, Device_To_Host),
                                                 6, 16#0100#, 0, 64),
                                      Req_EP => 0)),
                    --  Do NOT ACK the IN transfer with a ZLP, but send another
                    --  request instead.
                    3 => (Kind => Set_Verbose, Verbose => True),
                    4 =>  (Kind    => UDC_Event_E,
                           Evt     => (Kind   => Setup_Request,
                                       --  Get Device descriptor
                                       Req    => ((Dev, 0, Stand, Device_To_Host),
                                                  6, 16#0100#, 0, 64),
                                       Req_EP => 0))
                   );

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             RX_Data,
                                             Early_Address => False);

end Main;
