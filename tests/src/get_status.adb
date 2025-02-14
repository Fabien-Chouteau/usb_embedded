with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;

procedure Get_Status is
   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     UDC_Scenarios.Get_Status  (Verbose => True);

   RX_Data : aliased constant UInt8_Array := (1 .. 2 => 0);

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 0 Val: 0 Index: 0 Len: 2")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 2 bytes")
     .Append ("0000_0000_0000_0000: 00 00                                           ..")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 2")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> NONE");
begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => True);
end Get_Status;
