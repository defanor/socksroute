# socksroute #

A basic SOCKS5 proxy, with basic routing rules.

The primary purpose of this is to obtain access to some resources
(such as archive.org and .onion resources) without switching a web
browser or its settings, while accessing others directly. Though just
for .onion resources it may be more handy to
set
[transparent proxying](https://trac.torproject.org/projects/tor/wiki/doc/TransparentProxy#TransparentlyDoingDNSandRoutingfor.onionTraffic).

Currently there's not much of error reporting, exception handling, or
even logging, and probably things like Privoxy can do what it does;
this one is rather ad hoc, initially written after me being unable to
quickly find such a program.

For Firefox, extensions like FoxyProxy handle this task as well.

As of 2024, due to the increased DPI-assisted SNI-based censorship,
splitting of TLS records on provided substrings is added. Yet again,
there are other projects doing the same thing (see "Green Tunnel",
"zapret"), but I have not found anything simple and otherwise
satisfactory, being just a SOCKS5 server, without a GUI, and not doing
much else. Perhaps byedpi is close to that though.


## Running and installation ##

Simply `cabal run` to run it manually, or `cabal install`, possibly
copy the executable into a more appropriate location, install a
systemd service (an example one is provided).


## Configuration ##

Configuration is currently done in the code, which is short;
particularly in `domainRules` and `splitStrings`.

By default, it listens on `127.0.0.1:1080`, routes .onion resources
through Tor SOCKS with its default configuration (`localhost:9050`),
splits TLS records for googlevideo.com.

Something like "Remote DNS" should be set in a client program if the
rules rely on domain names.
