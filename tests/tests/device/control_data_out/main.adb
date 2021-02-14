--  This test is check control data out with a fake class request
with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL;            use HAL;
with HAL.USB;        use HAL.USB;
with HAL.USB.Device; use HAL.USB.Device;

with USB;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     Stub_Scenario'(1 => (True, (Kind   => Setup_Request,

                                 --  Use an invalid request to trigger a stall
                                 --  after the data transfer.
                                 Req     => ((Dev, 0, Class, Host_To_Device),
                                            42, 0, 0, 16),
                                 Req_EP  => 0)),
                    2 => (True, (Kind    => Data_Ready,
                                 RX_EP   => 0,
                                 RX_BCNT => 16)
                         )
                   );

   RX_Data : aliased constant UInt8_Array := (1 .. 16 => 42);

begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario, RX_Data);
end Main;
