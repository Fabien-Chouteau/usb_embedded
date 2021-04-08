--  This test is check control data out with a fake class request
with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;

with USB;            use USB;
with USB.Device;     use USB.Device;
with USB.HAL.Device; use USB.HAL.Device;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => True),
                    2 => (Kind => UDC_Event_E,
                          Evt  => (Kind   => Setup_Request,

                                   --  Use an invalid request to trigger a stall
                                   --  after the data transfer.
                                   Req     => ((Dev, 0, Class, Host_To_Device),
                                               42, 0, 0, 16),
                                   Req_EP  => 0)),
                    3 => (Kind => UDC_Event_E,
                          Evt  =>  (Kind => Transfer_Complete,
                                    EP   => (0, EP_Out),
                                    BCNT => 16)
                         )
                   );

   RX_Data : aliased constant UInt8_Array := (1 .. 16 => 42);

begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario, RX_Data);
end Main;
