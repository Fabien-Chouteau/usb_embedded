with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;

with USB;

procedure Main is

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

begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario, RX_Data);

end Main;
