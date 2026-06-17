with AUnit.Assertions;

with AAA.Strings;

with HAL; use HAL;
with USB_Testing.Output;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;

with USB; use USB;
with USB.Device.Serial;
with USB.Device; use USB.Device;

package body Tests.Device.Serial is

   ------------
   -- Serial --
   ------------

   procedure Serial (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        UDC_Scenarios.Set_Config (Verbose => False, Config_Id => 1) &
        Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => False),

                       --  Enable only to debug/investigate
                       2 => (Kind => Enable_Device_Log,
                             Dev_Log_Enabled => False),

                       --  Transfer serial data from host to device
                       3 => (Kind     => Transfer_Out,
                             EP_Out   => 2,
                             Data_Out => AAA.Strings.Empty_Vector.Append
                                          ("This is a test")),
                       4 =>  (Kind => UDC_Event_E,
                              Evt  => (Kind => Transfer_Complete,
                                       EP   => (2, EP_Out),
                                       BCNT => 14)),

                       5 => (Kind => Sync_Point, Sync_Id => 1),

                       --  Transfer serial data from host to device
                       6 => (Kind     => Transfer_Out,
                             EP_Out   => 2,
                             Data_Out => AAA.Strings.Empty_Vector
                             .Append ("Longer message.")
                             .Append ("Longer message.")
                             .Append ("Longer message.")
                             .Append ("Longer message.")
                            ),
                       7 =>  (Kind => UDC_Event_E,
                              Evt  => (Kind => Transfer_Complete,
                                       EP   => (2, EP_Out),
                                       BCNT => 4 * 15)),

                       8 => (Kind => Sync_Point, Sync_Id => 2)
                      );

      Serial_Class : aliased USB.Device.Serial.Default_Serial_Class (128, 128);

      Output : aliased USB_Testing.Output.Text_Output;

      UDC : aliased UDC_Stub.Controller (Output'Unchecked_Access,
                                         Scenario'Unchecked_Access,
                                         Has_Early_Address => False,
                                         Max_Packet_Size   => 64,
                                         EP_Buffers_Size   => 64 + 2 * 128,
                                         Number_Of_EPs     => 2,
                                         Init_Verbose      => False);
      Stack : USB.Device.USB_Device_Stack (Max_Classes => 1);
      Result : USB.Device.Init_Result;

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("Msg from Host => 14 'This is a test'")
        .Append ("Msg from Host => 60 'Longer message.Longer message.Longer message.Longer message.'")
        ;

      S : Natural := 1;

      A : Integer;
      From_Host : String (1 .. 128);
      Len : UInt32;
   begin

      AUnit.Assertions.Assert
        (Stack.Register_Class (Serial_Class'Unchecked_Access),
         "USB STACK register class failed");

      Result := Stack.Initialize
        (Controller      => UDC'Unchecked_Access,
         Manufacturer    => To_USB_String ("Manufacturer"),
         Product         => To_USB_String ("Product"),
         Serial_Number   => To_USB_String ("Serial"),
         Max_Packet_Size => 64);

      AUnit.Assertions.Assert
        (Result = Ok, "USB STACK Init failed: " & Result'Img);

      Stack.Start;

      loop
         Stack.Poll;

         case S is
            when 1 =>
               Serial_Class.Read (From_Host, Len);
               if Len /= 0 then
                  UDC.Put_Line_Always ("Msg from Host =>" & Len'Img & " '" &
                                         From_Host (1 .. Integer (Len)) & "'");
                  UDC.Signal_Sync_Point (S);
                  S := S + 1;
               end if;

            when 2 =>
               Serial_Class.Read (From_Host, Len);
               if Len /= 0 then
                  UDC.Put_Line_Always ("Msg from Host =>" & Len'Img & " '" &
                                         From_Host (1 .. Integer (Len)) & "'");
                  UDC.Signal_Sync_Point (S);
                  S := S + 1;
               end if;

            when 3 =>
               Len := 4;
               Serial_Class.Write (UDC, A'Address, Len);
               --  Serial_Class.Read (A'Address, Len);
               UDC.Put_Line ("Len:" & Len'Img);
               if Len /= 0 then
                  S := S + 1;
               end if;
            when others =>
               null;
         end case;

         pragma Warnings (Off, "possible infinite loop");
         exit when UDC.End_Of_Scenario;
         pragma Warnings (On, "possible infinite loop");
      end loop;

      declare
         use USB_Testing.Output;

         Actual : constant AAA.Strings.Vector := Output.Dump;
      begin
         AUnit.Assertions.Assert
           (Equal (Expected, Actual),
            "Diff in output: " & ASCII.LF &
              Diff (Expected, Actual, "Expected", "Actual").Flatten (ASCII.LF));

      end;

   end Serial;

begin

   Suite.Add_Test (UDC_Stub_Caller.Create ("Device Serial over USB",
                   Serial'Access));

end Tests.Device.Serial;
