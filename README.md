# Overview

A networked tic-tac-toe program for two human players.

Designed with a thin client and a fat server. Style and architecture are in progress. Currently, the user interface is not very friendly.

# Usage
[in three different terminals; or after it is on public internet, on three actually different computers]
> runhaskell server.hs [portnumber]
>
> runhaskell client.hs [portnumber]
>
> runhaskell client.hs [portnumber]

# TODOs
  - expose to public internet instead of hardcoding 0.0.0.0 in the client
  - better handling of user input when it is not their turn
