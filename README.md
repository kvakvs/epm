# Erlang Package Manager (epm)

This is client/server package manager for prebuilt binary Erlang packages, but
with source support. See requirements below.

Project is in development. _Do not use_

## Requirements

These are things I expect `epm` to be able to do:

### Client

*  Support rebar config and possibly own format for dependencies. Similar to ruby
   version lock file.
*  Be able to fetch binary and source packages; if commit hash is locked, then
   only source is available.
*  Store configs and cache of downloaded binary packages in user home

### Server

*  Binary repo running on a commodity web server with index and subdirectories
   like those Linux repos do;

### Future Plans

*  Store index in Git(hub) with pull-requests and signing