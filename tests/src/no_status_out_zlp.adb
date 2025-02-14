with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;
with USB; use USB;

procedure No_Status_Out_ZLP is
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

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 256 Index: 0 Len: 64")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 18 bytes")
     .Append ("0000_0000_0000_0000: 12 01 10 01 00 00 00 40 66 66 42 42 21 01 01 02 .......@ffBB!...")
     .Append ("0000_0000_0000_0010: 03 01                                           ..")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 18")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> NONE");
begin
   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => False);
end No_Status_Out_ZLP;
