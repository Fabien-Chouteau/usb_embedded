with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Scenarios;
with USB_Testing.UDC_Stub;
with USB_Testing; use USB_Testing;

procedure Device_Descriptor is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => True);

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Request_Buffer ([EP_IN 0], Len => 64)")
     .Append ("UDC Request_Buffer ([EP_OUT 0], Len => 64)")
     .Append ("UDC Initialize")
     .Append ("UDC Start")
     .Append ("UDC Poll -> RESET")
     .Append ("UDC Reset")
     .Append ("UDC EP_Setup [EP_IN 0] Type: CONTROL")
     .Append ("UDC EP_Setup [EP_OUT 0] Type: CONTROL")
     .Append ("UDC Set_Address 0")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Reset")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 256 Index: 0 Len: 64")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 18 bytes")
     .Append ("0000_0000_0000_0000: 12 01 10 01 00 00 00 40 66 66 42 42 21 01 01 02 .......@ffBB!...")
     .Append ("0000_0000_0000_0010: 03 01                                           ..")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 18")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
     .Append ("UDC Poll -> NONE");

begin
   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => False,
                                             Init_Verbose  => True);
end Device_Descriptor;
