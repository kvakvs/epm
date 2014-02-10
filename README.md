# Erlang Package Manager (epm)

This is client/server package manager for prebuilt binary Erlang packages, but
with source support. See requirements below.

Project is in development. *Do not use*

## Requirements

These are things I expect `epm` to be able to do:

### TODO Client

*  Fetching by branch, tag/version, hash (source only).
*  Ability to list URLs pointing to archive/tarball as dependency.
*  [?] Central installation to erlang/lib

### TODO Server

*  Binary repo running on a commodity web server with index and subdirectories
   like those Linux repos do;

### Future Plans

*  Store index in Git(hub) with pull-requests and signing

## Use Cases

TODO: `epm ls` - list installed packages (locally and globally?)
TODO: `epm update`

## Package

A package is (copy from npm docs):

*   (a) a folder containing a program described by a package.json file
*   (b) a gzipped tarball containing (a)
*   (c) a url that resolves to (b)
*   (d) a <name>@<version> that is published on the registry with (c)
*   (e) a <name>@<tag> that points to (d)
*   (f) a <name> that has a "latest" tag satisfying (e)
*   (g) a git url that, when cloned, results in (a).

### Command Line

Copy from npm docs:

*   `npm install (with no args in a package dir)` -- (in package directory, no
    arguments): Install the dependencies in the local node_modules folder.
*   `npm install <tarball file>` -- Install a package that is sitting on the
    filesystem.
*   `npm install <tarball url>` -- Fetch the tarball url, and then install it.
    In order to distinguish between this and other options, the argument must
    start with "http://" or "https://"
*   `npm install <folder>` -- Install a package that is sitting in a folder on
    the filesystem.
*   `npm install <name> [--save|--save-dev|--save-optional]` -- Do a <name>@<tag>
    install, where <tag> is the "tag" config. (?)
*   `npm install <name>@<tag>` -- Install the version of the package that is
    referenced by the specified tag. If the tag does not exist in the registry
    data for that package, then this will fail.
*   `npm install <name>@<version>` -- Install the specified version of the
    package. This will fail if the version has not been published to the registry.
*   `npm install <name>@<version range>` -- Install a version of the package
    matching the specified version range. This will follow the same rules for
    resolving dependencies described in package.json(5).
*   `npm install <git remote url>` -- Install a package by cloning a git
    remote url.
*   `npm i (with any of the previous argument usage)`

## Algorithm

To install a package, npm uses the following algorithm:

install(where, what, family, ancestors)

fetch what, unpack to <where>/node_modules/<what>

for each dep in what.dependencies
-   resolve dep to precise version

for each dep@version in what.dependencies
-   not in <where>/node_modules/<what>/node_modules/*
-   and not in <family>
  -  add precise version deps to <family>
  -  install(<where>/node_modules/<what>, dep, family)

For this package{dep} structure: A{B,C}, B{C}, C{D}, this algorithm produces:

```
A
+-- B
`-- C
    `-- D
```

That is, the dependency from B to C is satisfied by the fact that A already
caused C to be installed at a higher level.
