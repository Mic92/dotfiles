require 'socket'

TCPSocket.open('home.devkid.net', 22198) do |socket|
  socket.write(File.read("/run/keys/nas-wakeup-password"))
end
