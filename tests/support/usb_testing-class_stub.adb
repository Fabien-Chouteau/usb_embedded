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

with Ada.Text_IO; use Ada.Text_IO;
with Hex_Dump;
with USB; use USB;

package body USB_Testing.Class_Stub is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This            : in out Device_Class_Stub;
                         Dev             : in out USB_Device;
                         Interface_Index :        Class_Index)
   is
   begin
      if not Dev.Request_Endpoint (This.EP) then
         raise Program_Error with "Cannot get EP for stub class " &
           This.Number'Img;
      end if;
      This.Interface_Index := Interface_Index;
   end Initialize;

   ------------------------------
   -- Config_Descriptor_Length --
   ------------------------------

   overriding
   function Config_Descriptor_Length (This : in out Device_Class_Stub)
                                      return Positive
   is
   begin
      return 50;
   end Config_Descriptor_Length;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Device_Class_Stub;
                                     Data :    out UInt8_Array)
   is
      pragma Unreferenced (Data);
   begin
      Data := (others => This.Interface_Index);
   end Fill_Config_Descriptor;

   ---------------
   -- Configure --
   ---------------

   overriding
   function Configure (This  : in out Device_Class_Stub;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return USB.Setup_Request_Answer
   is
   begin
      Put_Line ("USB Class" & This.Number'Img &
                  " Configure Index:" & Index'Img);

      UDC.EP_Setup (EP       => (This.Ep, EP_Out),
                    Typ      => Interrupt,
                    Max_Size => 4,
                    Callback => null);

      return USB.Handled;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Device_Class_Stub;
                                Req   : USB.Setup_Data;
                                Buf   : out System.Address;
                                Len   : out USB.Buffer_Len)
                                return USB.Setup_Request_Answer
   is
      pragma Unreferenced (Buf, Len);
   begin
      Put_Line ("USB Class" & This.Number'Img &
                  " Setup_Request " & USB.Img (Req));

      return USB.Next_Callback;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Device_Class_Stub;
                                 Req   : USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return USB.Setup_Request_Answer
   is
   begin
      Put_Line ("USB Class" & This.Number'Img &
                  " Setup_Request " & USB.Img (Req));

      Hex_Dump.Hex_Dump (Data, Put_Line'Access);

      return USB.Next_Callback;
   end Setup_Write_Request;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Device_Class_Stub;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : USB.EP_Addr)
   is
      pragma Unreferenced (UDC);
   begin
      if EP.Num /= This.Ep then
         raise Program_Error with "Unexpected Endpoint";
      end if;

      Put_Line ("USB Class" & This.Number'Img &
                  " Transfer_Complete " &
                  USB.Img (EP));
   end Transfer_Complete;

   ----------------
   -- Data_Ready --
   ----------------

   overriding
   procedure Data_Ready (This : in out Device_Class_Stub;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : USB.EP_Id;
                         BCNT : UInt32)
   is
      pragma Unreferenced (UDC);
   begin
      if EP /= This.Ep then
         raise Program_Error with "Unexpected Endpoint";
      end if;

      Put_Line ("USB Class" & This.Number'Img & " Data_Ready " &
                  USB.Img (USB.EP_Addr'(EP, USB.EP_Out)) &
                  "BCNT:" & BCNT'Img);
   end Data_Ready;

end USB_Testing.Class_Stub;
