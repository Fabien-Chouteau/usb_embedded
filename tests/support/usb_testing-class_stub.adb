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

with USB; use USB;

package body USB_Testing.Class_Stub is

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (This            : in out Device_Class_Stub;
                        Dev             : in out USB_Device_Stack'Class;
                        Interface_Index :        Interface_Id)
                        return Init_Result
   is
   begin
      if not Dev.Request_Endpoint (Bulk, This.Ep) then
         This.Output.Put_Line ("Cannot get EP for stub class " & This.Number'Img);
         return Not_Enough_EPs;
      end if;
      This.Interface_Index :=  Interface_Index;

      return Ok;
   end Initialize;

   --------------------
   -- Get_Class_Info --
   --------------------

   overriding
   procedure Get_Class_Info
     (This                     : in out Device_Class_Stub;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural)
   is
      pragma Unreferenced (This);
   begin
      Number_Of_Interfaces := 1;
      Config_Descriptor_Length := 50;
   end Get_Class_Info;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Device_Class_Stub;
                                     Data :    out UInt8_Array)
   is
      pragma Unreferenced (Data);
   begin
      Data := (others => UInt8 (This.Interface_Index));
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
      This.Output.Put_Line ("USB Class" & This.Number'Img &
                              " Configure Index:" & Index'Img);

      UDC.EP_Setup (EP  => (This.Ep, EP_Out),
                    Typ => Interrupt);

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
      This.Output.Put_Line ("USB Class" & This.Number'Img &
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
      This.Output.Put_Line ("USB Class" & This.Number'Img &
                              " Setup_Request " & USB.Img (Req));

      This.Output.Hex_Dump (Data);

      return USB.Next_Callback;
   end Setup_Write_Request;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Device_Class_Stub;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        USB.EP_Addr;
                                CNT  :        USB.Packet_Size)
   is
      pragma Unreferenced (UDC);
   begin
      if EP.Num /= This.Ep then
         raise Program_Error with "Unexpected Endpoint";
      end if;

      This.Output.Put_Line ("USB Class" & This.Number'Img &
                              " Transfer_Complete " &
                              USB.Img (EP) & " BCNT:" & CNT'Img);
   end Transfer_Complete;

end USB_Testing.Class_Stub;
