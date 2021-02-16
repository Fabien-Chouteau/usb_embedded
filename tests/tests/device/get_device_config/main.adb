with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;

with USB;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     UDC_Scenarios.Set_Address (Verbose => False, Addr => 42) &
     UDC_Scenarios.Get_Config (Verbose => True);

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

begin

   USB_Testing.UDC_Scenarios.Two_Classes_UDC_Test (Scenario, RX_Data);

end Main;
