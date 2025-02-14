with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;

procedure String_Descriptor is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False)
     & UDC_Scenarios.Get_Config (Verbose => False)

     --  Get string #1 usually manufacturer from device desc
     & UDC_Scenarios.Get_String (Verbose => True, Id => 1,  Ack => True)
     --  Get string #2 usually product from device desc
     & UDC_Scenarios.Get_String (Verbose => True, Id => 2,  Ack => True)
     --  Get string #3 usually serial number from device desc
     & UDC_Scenarios.Get_String (Verbose => True, Id => 3,  Ack => True)
     --  Get invalid string
     & UDC_Scenarios.Get_String (Verbose => True, Id => 42, Ack => False);

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 769 Index: 0 Len: 255")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 26 bytes")
     .Append ("0000_0000_0000_0000: 1A 03 4D 00 61 00 6E 00 75 00 66 00 61 00 63 00 ..M.a.n.u.f.a.c.")
     .Append ("0000_0000_0000_0010: 74 00 75 00 72 00 65 00 72 00                   t.u.r.e.r.")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 26")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 770 Index: 0 Len: 255")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 16 bytes")
     .Append ("0000_0000_0000_0000: 10 03 50 00 72 00 6F 00 64 00 75 00 63 00 74 00 ..P.r.o.d.u.c.t.")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 16")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 771 Index: 0 Len: 255")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Write_Packet [EP_IN 0] 14 bytes")
     .Append ("0000_0000_0000_0000: 0E 03 53 00 65 00 72 00 69 00 61 00 6C 00       ..S.e.r.i.a.l.")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 14")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 810 Index: 0 Len: 255")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Stall [EP_IN 0] TRUE")
     .Append ("UDC EP_Stall [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> NONE");
begin
   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             Expected,
                                             RX_Data,
                                             Early_Address => True);
end String_Descriptor;
