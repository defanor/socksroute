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
this one is rather ad hoc, written after me being unable to quickly
find such a program. But it works fine.


## Installation ##

To build the program in a cabal sandbox and install a systemd service,
`make install`. To uninstall it, `make uninstall`.


## Configuration ##

Configuration is currently getting done in the code, which is short;
particularly in the `domainRules` function.

By default, it listens on `127.0.0.1:1080`, and routes .onion and
google resources (as well as a couple of others) through Tor SOCKS
with its default configuration (`localhost:9050`).

Something like "Remote DNS" should be set in a client program if the
rules rely on domain names.
