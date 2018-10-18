#!/home/swish/bin/swipl

:- use_module(library(settings)).
:- use_module(library(http/http_log)).
:- set_setting_default(http:logfile, 'log/httpd.log').

:- use_module(library(http/http_unix_daemon)).

:- [proxy].
