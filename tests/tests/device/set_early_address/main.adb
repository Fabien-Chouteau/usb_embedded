--  Some USB Device controllers require the address to be set earlier than
--  the standard calls for. That is, before the set_address setup request
--  is acknoledged with a Zero-Lenght-Packet.

with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     UDC_Scenarios.Set_Address (Verbose => True);

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

begin

   USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                             RX_Data,
                                             Early_Address => True);

end Main;
