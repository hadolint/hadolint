module Messages where

import Analyzer

message :: Rule -> String
message NoSudo = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
message ExplicitTag = "Always specify base images explicitly. Instead of `FROM debian` use `FROM debian:jessie`."
message NoUpgrade = "Do not use apt-get upgrade or dist-upgrade."
message WgetAndCurlUsed = "Either use curl or wget for downloading to
save dependencies."
message UnusableCommand = "For some bash commands it makes no sense running them
in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
