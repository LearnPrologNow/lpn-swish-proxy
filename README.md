# LPN proxy server

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

