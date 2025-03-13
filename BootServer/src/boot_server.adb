with Ada.Text_IO;
with Ada.Command_Line;
with Config;
with Network.Sockets;
with Protocols.BOOTP;
with Protocols.TFTP;
with Utils.Logging;

procedure Boot_Server is
   use Ada.Text_IO;
   use Utils.Logging;

   Config_File : String := "config/boot_server.conf";
begin
   if Ada.Command_Line.Argument_Count >= 1 then
      Config_File := Ada.Command_Line.Argument(1);
   end if;

   Log_Info("Boot server starting...");

   Config.Load(Config_File);
   Log_Info("Configuration loaded from " & Config_File);

   Network.Sockets.Initialize;
   Log_Info("Network initialized");

   Protocols.BOOTP.Start;
   Protocols.TFTP.Start;
   Log_Info("Protocol handlers started");

   loop
      Protocols.BOOTP.Process_Requests;
      Protocols.TFTP.Process_Requests;
      delay 0.01
   end loop

exception
   when E : others =>
      Log_Error("Unhandled exception: " & Exception_Information(E));
      Put_Line("Server terminated due to unhandled exception");
end Boot_Server;
