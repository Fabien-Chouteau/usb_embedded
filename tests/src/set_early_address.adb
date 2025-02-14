with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;

procedure Set_Early_Address is
   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     UDC_Scenarios.Set_Address (Verbose => True);

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,DEV) Req: 5 Val: 42 Index: 0 Len: 0")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC Set_Address 42")
     .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
     .Append ("UDC Set_Address 42")
     .Append ("UDC Poll -> NONE");

begin
   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => True);
end Set_Early_Address;
