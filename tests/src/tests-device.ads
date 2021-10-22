with AUnit;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

package Tests.Device is

   type UDC_Stub_Fixture is new AUnit.Test_Fixtures.Test_Fixture with record
      A : Integer;
   end record;

   overriding
   procedure Set_Up (T : in out UDC_Stub_Fixture);

private

   package UDC_Stub_Caller is new AUnit.Test_Caller (UDC_Stub_Fixture);

end Tests.Device;
