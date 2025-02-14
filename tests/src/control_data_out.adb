with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;
with USB; use USB;

procedure Control_Data_Out is
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

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,CLASS,DEV) Req: 42 Val: 0 Index: 0 Len: 16")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 16")
     .Append ("UDC OUT Transfer [EP_OUT 0] 16 bytes")
     .Append ("0000_0000_0000_0000: 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A ****************")
     .Append ("USB Class 1 Setup_Request Type: (HOST_TO_DEVICE,CLASS,DEV) Req: 42 Val: 0 Index: 0 Len: 16")
     .Append ("0000_0000_0000_0000: 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A ****************")
     .Append ("UDC EP_Stall [EP_IN 0] TRUE")
     .Append ("UDC EP_Stall [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> NONE");

begin
   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => True);
end Control_Data_Out;
