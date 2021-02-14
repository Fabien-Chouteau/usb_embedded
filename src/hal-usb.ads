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

package HAL.USB is

   subtype EP_Id is UInt4;

   type EP_Dir is (EP_In, EP_Out);

   type EP_Addr is record
      Num : EP_Id;
      Dir : EP_Dir;
   end record;

   type EP_Type is (Control, Isochronous, Bulk, Interrupt);
   for EP_Type use (Control     => 0,
                    Isochronous => 1,
                    Bulk        => 2,
                    Interrupt   => 3);

   type Data_Phase_Transfer_Direction is (Host_To_Device,
                                          Device_To_Host)
     with Size => 1;

   for Data_Phase_Transfer_Direction use (Host_To_Device => 0,
                                          Device_To_Host => 1);

   type Request_Type_Type is (Stand, Class, Vendor, Reserved)
     with Size => 2;
   for Request_Type_Type use (Stand    => 0,
                              Class    => 1,
                              Vendor   => 2,
                              Reserved => 3);

   type Request_Type_Recipient is (Dev, Iface, Endpoint, Other);
   for Request_Type_Recipient use (Dev      => 0,
                                   Iface    => 1,
                                   Endpoint => 2,
                                   Other    => 3);
   type Request_Type is record
      Recipient : Request_Type_Recipient;
      Reserved  : UInt3;
      Typ : Request_Type_Type;
      Dir : Data_Phase_Transfer_Direction;
   end record with Pack, Size => 8;

   type Setup_Data is record
      RType   : Request_Type;
      Request : UInt8;
      Value   : UInt16;
      Index   : UInt16;
      Length  : UInt16;
   end record with Pack, Size => 8 * 8;

   function Img (D : Setup_Data) return String
   is ("Type: (" & D.RType.Dir'Img & "," & D.RType.Typ'Img & "," &
         D.RType.Recipient'Img & ")" &
         " Req:" & D.Request'Img &
         " Val:" & D.Value'Img &
         " Index:" & D.Index'Img &
         " Len:" & D.Length'Img);

   function Img (EP : EP_Addr) return String
   is ("["  & EP.Dir'Img & EP.Num'Img & "]");

end HAL.USB;
