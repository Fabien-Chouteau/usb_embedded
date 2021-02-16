------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

private package USB.Device.Control is

   procedure Setup (This : in out USB_Device;
                    EP   : EP_Id);
   --  Handle setup request

   procedure Control_In (This : in out USB_Device);

   procedure Control_Out (This : in out USB_Device;
                          BCNT : UInt11);
   --  FIXME: Is BCNT useful?

private

   procedure Setup_Read (This : in out USB_Device);
   --  Handle setup read request

   procedure Setup_Write (This : in out USB_Device);
   --  Handle setup write request

   function Dispatch_Request (This : in out USB_Device)
                              return Setup_Request_Answer;

   function Dispatch_Request_To_Class (This : in out USB_Device)
                                       return Setup_Request_Answer;

   procedure Handle_Read_Request (This : in out USB_Device);
   --  Handle setup read request

   procedure Handle_Write_Request (This : in out USB_Device);
   --  Handle setup write request

   function Device_Request (This : in out USB_Device)
                            return Setup_Request_Answer;

   procedure Send_Chunk (This : in out USB_Device);

   procedure Receive_Chunk (This : in out USB_Device);

end USB.Device.Control;
