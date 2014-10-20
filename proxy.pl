:- module(lpn_proxy,
	  [ server/1
	  ]).
:- use_module(convert).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).

user:file_search_path(lpn, .).
user:file_search_path(lpn_cache, lpn(cache)).

:- http_handler('/', serve_files_in_directory(lpn), [prefix]).
:- http_handler('/lpnpage.php', lpn, []).

server(Port) :-
	http_server(http_dispatch,
		    [ port(Port)
		    ]).

lpn(Request) :-
	option(request_uri(URI), Request),
	pageid(URI, PageID),
	(   absolute_file_name(lpn_cache(PageID), Path,
			       [access(read), file_errors(fail)])
	->  reply_from_file(Path)
	;   absolute_file_name(lpn_cache(PageID), Path,
			       [access(write), file_errors(fail)])
	->  download(URI, Path),
	    reply_from_file(Path)
	;   atom_concat('http://www.learnprolognow.org', URI, Source),
	    setup_call_cleanup(
		http_open(Source, In, []),
		reply_from_stream(In),
		close(In))
	).

pageid(URI, PageID) :-
	uri_components(URI, Components),
	uri_data(search, Components, Search),
	nonvar(Search),
	uri_query_components(Search, Query),
	memberchk(pageid=PageID, Query).

reply_from_file(Path) :-
	setup_call_cleanup(
	    open(Path, read, In),
	    reply_from_stream(In),
	    close(In)).

download(URI, Path) :-
	atom_concat('http://www.learnprolognow.org', URI, Source),
	setup_call_cleanup(
	    http_open(Source, In, []),
	    setup_call_cleanup(
		open(Path, write, Out, [type(binary)]),
		copy_stream_data(In, Out),
		close(Out)),
	    close(In)).

reply_from_stream(In) :-
	format('Content-type: text/html~n~n'),
	convert_lpn(In, current_output).
