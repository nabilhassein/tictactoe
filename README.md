# Overview

A networked tic-tac-toe program for two human players.

Design goal was a thin client and a fat server, but server is pretty slender as well. (Ask me about the picture I drew to design the program!) Style and architecture are in progress. Currently, the user interface is not very friendly.

Simply run

> runhaskell client.hs

with no arguments, and you will join the next available game. There is no message while you wait. If you get tired of waiting, you can open a second terminal window to confirm that the program works.

There is no option to select your opponent.

# Usage

> runhaskell client.hs [hostname] [portnumber]

And then at a different computer, or at least a different terminal, the same command again:

> runhaskell client.hs [hostname] [portnumber]

The hostname and portnumber are optional. You will connect to nabilhassein.com if you don't specify them.

If you supply only one argument, it is assumed to be a hostname. In this case the default port is 8000.

You can also run your own server with

> runhaskell server.hs [portnumber]

If you do this, then be sure to either change the default hostname (and probably port number) in your local version of server.hs and client.hs; or else specify the correct hostname and port number on the command line when you run your clients.

The port number in use on the server running at nabilhassein.com is 443 because I'm currently too lazy to configure my firewall.

# TODOs
  - better handling of user input when it is not their turn
  - configure firewall and stop using port 443
