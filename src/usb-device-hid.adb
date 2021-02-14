------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL.USB; use HAL.USB;

with Ada.Unchecked_Conversion;

package body USB.Device.HID is

   HID_Mouse_Report_Desc : aliased constant UInt8_Array :=
     (
      16#05#,   16#01#,
      16#09#,   16#02#,
      16#A1#,   16#01#,
      16#09#,   16#01#,
      16#A1#,   16#00#,
      16#05#,   16#09#,
      16#19#,   16#01#,
      16#29#,   16#03#,
      16#15#,   16#00#,
      16#25#,   16#01#,
      16#95#,   16#03#,
      16#75#,   16#01#,
      16#81#,   16#02#,
      16#95#,   16#01#,
      16#75#,   16#05#,
      16#81#,   16#01#,
      16#05#,   16#01#,
      16#09#,   16#30#,
      16#09#,   16#31#,
      16#09#,   16#38#,
      16#15#,   16#81#,
      16#25#,   16#7F#,
      16#75#,   16#08#,
      16#95#,   16#03#,
      16#81#,   16#06#,
      16#C0#,   16#09#,
      16#3c#,   16#05#,
      16#ff#,   16#09#,
      16#01#,   16#15#,
      16#00#,   16#25#,
      16#01#,   16#75#,
      16#01#,   16#95#,
      16#02#,   16#b1#,
      16#22#,   16#75#,
      16#06#,   16#95#,
      16#01#,   16#b1#,
      16#01#,   16#c0#
     );

   ---------------
   -- Configure --
   ---------------

   overriding function Configure
     (This  : in out Default_HID_Class;
      UDC   : in out USB_Device_Controller'Class;
      Index : UInt16)
      return Setup_Request_Answer
   is
      pragma Unreferenced (This);
   begin
      if Index = 1 then

         UDC.EP_Setup (EP       => (1, EP_In),
                       Typ      => Interrupt,
                       Max_Size => 4,
                       Callback => null);

         This.State := Idle;
         return Handled;
      else
         return Not_Supported;
      end if;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Default_HID_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer
   is
   begin
      Buf := System.Null_Address;
      Len := 0;

      if Req.RType.Typ = Class and then Req.RType.Recipient = Iface then
         case Req.Request is
         when 1 => -- GET_REPORT
            return Not_Supported;
         when 2 => -- GET_IDLE
            return Not_Supported;
         when 3 => -- GET_PROTOCOL
            return Not_Supported;
         when 9 => -- SET_REPORT
            return Not_Supported;
         when 10 => -- SET_IDLE
            This.Idle_State := UInt8 (Shift_Right (Req.Value, 8) and 16#FF#);
            return Handled;
         when 11 => -- SET_PROTOCOL
            return Not_Supported;
         when others =>
            raise Program_Error with "Unknown HID requset";
         end case;
      end if;

      if Req.RType.Typ = Stand
        and then
          Req.Request = 6 -- GET_DESCRIPTOR
      then
         declare
--              Index     : constant UInt8 := UInt8 (Req.Value and 16#FF#);
            Desc_Type : constant UInt8 :=
              UInt8 (Shift_Right (Req.Value, 8) and 16#FF#);

         begin
            case Desc_Type is
               when 16#22# => --  HID_REPORT_DESC
                  Buf := HID_Mouse_Report_Desc'Address;
                  Len := Buffer_Len (HID_Mouse_Report_Desc'Length);
                  return Handled;
               when others =>
                  raise Program_Error with "Unknown desc in HID class";
            end case;
         end;
      end if;

      raise Program_Error with "You have to implement stuff here";
      return Not_Supported;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Default_HID_Class;
                                 Req   : HAL.USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is (Not_Supported);

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Default_HID_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : HAL.USB.EP_Addr)
   is
   begin
      if EP = (1, EP_In) then
         This.State := Idle;

         --  Setup for next TX
         UDC.EP_Setup (EP       => (1, EP_In),
                       Typ      => Interrupt,
                       Max_Size => 4,
                       Callback => null);
      end if;
   end Transfer_Complete;

   ----------------
   -- Data_Ready --
   ----------------

   overriding
   procedure Data_Ready (This : in out Default_HID_Class;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : HAL.USB.EP_Id;
                         BCNT : UInt32)
   is
   begin
      raise Program_Error with "Not expecting data...";
   end Data_Ready;

   --------------
   -- Set_Move --
   --------------

   procedure Set_Move (This : in out Default_HID_Class;
                       X, Y : Interfaces.Integer_8)
   is
      function To_UInt8 is new Ada.Unchecked_Conversion (Interfaces.Integer_8,
                                                         UInt8);
   begin
      This.Report (2) := To_UInt8 (X);
      This.Report (3) := To_UInt8 (Y);
   end Set_Move;

   -----------------
   -- Send_Report --
   -----------------

   procedure Send_Report (This : in out Default_HID_Class;
                          UDC  : in out USB_Device_Controller'Class)
   is
   begin
      if This.Ready then
         UDC.EP_Write_Packet (1,
                              This.Report'Address,
                              UInt32 (This.Report'Length));
         This.State := Busy;
      end if;
   end Send_Report;

   -----------
   -- Ready --
   -----------

   function Ready (This : in out Default_HID_Class) return Boolean
   is (This.State = Idle);

end USB.Device.HID;
