------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with System; use System;

with USB.Utils;

package body USB.Device.HID is

   subtype Dispatch is Abstract_HID_Class'Class;

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (This                 : in out Abstract_HID_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result
   is
   begin
      if not Dev.Request_Endpoint (Interrupt, This.EP) then
         return Not_Enough_EPs;
      end if;

      This.Report_Buf := Dev.Request_Buffer ((This.EP, EP_Out),
                                             UInt11 (This.Report_Size));
      if This.Report_Buf = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      This.Interface_Index := Base_Interface_Index;

      return Ok;
   end Initialize;

   --------------------
   -- Get_Class_Info --
   --------------------

   overriding
   procedure Get_Class_Info
     (This                     : in out Abstract_HID_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural)
   is
      pragma Unreferenced (This);
   begin
      Number_Of_Interfaces := 1;
      Config_Descriptor_Length := 25;
   end Get_Class_Info;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Abstract_HID_Class;
                                     Data :    out UInt8_Array)
   is
      F : constant Natural := Data'First;

      USB_DESC_TYPE_INTERFACE     : constant := 4;
      USB_DESC_TYPE_ENDPOINT      : constant := 5;

   begin
      Data (F + 0 .. F + 24) :=
        (9,
         USB_DESC_TYPE_INTERFACE,
         0, --  This.Interface_Index,
         0, -- Alternate setting
         1, -- Number of endpoints
         3, -- Class HID
         0, -- Subclass
         0, -- Interface protocol 0=none, 1=keyboard, 2=mouse
         0, -- Str

         9,
         16#21#,
         16#11#, 16#01#, --  Class spec release number

         0,
         1,
         16#22#,
         Dispatch (This).Report_Descriptor'Length, 0, -- Descriptor length

         7,
         USB_DESC_TYPE_ENDPOINT,
         16#80# or UInt8 (This.EP), -- In EP
         3, -- Interrupt EP
         16#40#, 0, --  TODO: Max packet size
         1 -- Polling interval
        );
   end Fill_Config_Descriptor;

   ---------------
   -- Configure --
   ---------------

   overriding
   function Configure
     (This  : in out Abstract_HID_Class;
      UDC   : in out USB_Device_Controller'Class;
      Index : UInt16)
      return Setup_Request_Answer
   is
   begin
      if Index = 1 then

         UDC.EP_Setup (EP       => (This.EP, EP_In),
                       Typ      => Interrupt,
                       Max_Size => UInt16 (This.Report_Size));
         return Handled;
      else
         return Not_Supported;
      end if;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Abstract_HID_Class;
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
         when others =>
            raise Program_Error with "Unknown HID request";
         end case;
      end if;

      if Req.RType.Typ = Stand
        and then
          Req.Request = 6 -- GET_DESCRIPTOR
      then
         declare
--              Index     : constant UInt8 := UInt8 (Req.Value and 16#FF#);
            Desc_Type : constant UInt8 := Utils.High (Req.Value);

         begin
            case Desc_Type is
               when 16#22# => --  HID_REPORT_DESC
                  declare
                     Report : constant not null Report_Descriptor_Access
                       := Dispatch (This).Report_Descriptor;
                  begin
                     Buf := Report.all'Address;
                     Len := Buffer_Len (Report.all'Length);
                  end;

                  This.State := Idle;

                  return Handled;
               when others =>
                  raise Program_Error with "Unknown desc in HID class";
            end case;
         end;
      end if;

      return Next_Callback;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Abstract_HID_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is
   begin
      if Req.RType.Typ = Class and then Req.RType.Recipient = Iface then
         case Req.Request is
         when 9 => -- SET_REPORT
            declare
               Typ : constant UInt8 := Utils.High (Req.Value);
               ID  : constant UInt8 := Utils.Low (Req.Value);
            begin
               return Dispatch (This).Set_Report (Typ, ID, Data);
            end;
         when 10 => -- SET_IDLE
            This.Idle_State := Utils.High (Req.Value);
            return Handled;
         when 11 => -- SET_PROTOCOL
            return Not_Supported;
         when others =>
            raise Program_Error with "Unknown HID request";
         end case;
      end if;

      return Next_Callback;
   end Setup_Write_Request;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Abstract_HID_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        UInt11)
   is
   begin
      pragma Assert (EP.Num = This.EP);

      if EP = (This.EP, EP_In) then
         This.State := Idle;

         --  Setup for next TX
         UDC.EP_Setup (EP       => (This.EP, EP_In),
                       Typ      => Interrupt,
                       Max_Size => UInt16 (This.Report_Size));
      else
         raise Program_Error with "Not expecting transfer on EP";
      end if;
   end Transfer_Complete;

   -----------------
   -- Send_Report --
   -----------------

   procedure Send_Report (This : in out Abstract_HID_Class;
                          UDC  : in out USB_Device_Controller'Class)
   is
      Report : UInt8_Array (This.Report'Range)
        with Address => This.Report_Buf;

   begin
      if This.Ready then

         --  Copy the report to the transfer buffer
         Report := This.Report;

         --  Clear report
         This.Report := (others => 0);

         --  Send transfer buffer
         UDC.EP_Write_Packet (This.EP,
                              This.Report_Buf,
                              UInt32 (This.Report_Size));
         This.State := Busy;
      end if;
   end Send_Report;

   -----------
   -- Ready --
   -----------

   function Ready (This : in out Abstract_HID_Class) return Boolean
   is (This.State = Idle);

end USB.Device.HID;
