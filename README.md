# LPN proxy server

This repository provides the SWI-Prolog sources for a proxy server for
[Learn Prolog Now!](http://www.learnprolognow.org/) which rewrites the
sources to embed [SWISH](http://swish.swi-prolog.org/), the SWI-Prolog
web shell. The modified version of Learn Prolog Now is hosted at
[lpn.swi-prolog.org](http://lpn.swi-prolog.org)

The current version is merely a proof of concept. It identifies verbatim
environments in the Learn Prolog Now HTML and classifies these as source
code or example queries. It notably is not (yet) very good at
discovering the relations between source fragments. Most of the
infrastructure to improve on that is already in place: sources and
queries are parsed and analysed for predicates they implement and
require.

## Run locally

    % swipl proxy.pl
    ?- server(8080).

## Use local server at port 8000 rather than www.learnprolognow.org:

    % swipl proxy.pl
    ?- local.
    ?- server(8080).

## Use local SWISH at port 3050 rather than swish.swi-prolog.org

    ?- local_swish(3050).

## Run as daemon

As current user

    % ./daemon.pl --port=8080

As defined user

    % sudo -u www-data ./daemon.pl --port=8080

Or on port 80:

    % sudo ./daemon.pl --user=www-data

## Enable caching

Create a directory `cache` and make sure  it is writeable by the server.
If no such diretory exists, all pages are fetched dynamically.

    % mkdir cache
    % sudo chgrp www-data cache
    % sudo chmod g+w cache

