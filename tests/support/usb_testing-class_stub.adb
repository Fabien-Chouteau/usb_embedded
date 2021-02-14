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

package body USB_Testing.Class_Stub is


   ---------------
   -- Configure --
   ---------------

   overriding
   function Configure (This  : in out Device_Class_Stub;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return USB.Setup_Request_Answer
   is
      pragma Unreferenced (This, UDC);
   begin
      Put_Line ("USB Class Configure Index:" & Index'Img);
      return USB.Handled;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Device_Class_Stub;
                                Req   : HAL.USB.Setup_Data;
                                Buf   : out System.Address;
                                Len   : out USB.Buffer_Len)
                                return USB.Setup_Request_Answer
   is
      pragma Unreferenced (This, Buf, Len);
   begin
      Put_Line ("USB Class Setup_Request " & Img (Req));
      return USB.Next_Callback;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Device_Class_Stub;
                                 Req   : HAL.USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return USB.Setup_Request_Answer
   is
      pragma Unreferenced (This);
   begin
      Put_Line ("USB Class Setup_Request " & Img (Req));
      Hex_Dump.Hex_Dump (Data, Put_Line'Access);
      return USB.Next_Callback;
   end Setup_Write_Request;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Device_Class_Stub;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : HAL.USB.EP_Addr)
   is
      pragma Unreferenced (This, UDC);
   begin
      Put_Line ("USB Class Transfer_Complete " & Img (EP));
   end Transfer_Complete;

   ----------------
   -- Data_Ready --
   ----------------

   overriding
   procedure Data_Ready (This : in out Device_Class_Stub;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : HAL.USB.EP_Id;
                         BCNT : UInt32)
   is
      pragma Unreferenced (This, UDC);
   begin
      Put_Line ("USB Class Data_Ready " & Img (EP_Addr'(EP, HAL.USB.EP_Out)) &
                  "BCNT:" & BCNT'Img);
   end Data_Ready;

end USB_Testing.Class_Stub;
