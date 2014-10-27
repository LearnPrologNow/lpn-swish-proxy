:- module(lpn_proxy,
	  [ local/0,
	    server/1			% +Port
	  ]).
:- use_module(convert).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_open)).

/** <module> Learn Prolog Now proxy

This module implements a simple proxy that rewrites LPN to link to SWISH
*/

user:file_search_path(lpn, .).
user:file_search_path(lpn_cache, lpn(cache)).

:- setting(lpn_home, atom,
	   'http://www.learnprolognow.org',
	   'The location from which we proxy').

local :-
	set_setting(lpn_home, 'http://localhost:8000').

:- http_handler('/', http_redirect(moved_temporary,
				   '/lpnpage.php?pageid=online'), []).
:- http_handler('/', serve_files_in_directory(lpn), [prefix]).
:- http_handler('/lpnpage.php', lpn, []).
:- http_handler('/html/', pics, [prefix]).

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
	;   setting(lpn_home, LPNHome),
	    atom_concat(LPNHome, URI, Source),
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

pics(Request) :-
	option(path_info(Rest), Request),
	(   absolute_file_name(lpn_cache(Rest), _,
			       [access(read), file_errors(fail)])
	->  http_reply_file(lpn_cache(Rest), [], Request)
	;   absolute_file_name(lpn_cache(Rest), Path,
			       [access(write), file_errors(fail)])
	->  option(request_uri(URI), Request),
	    download(URI, Path),
	    http_reply_file(lpn_cache(Rest), [], Request)
	;   option(request_uri(URI), Request),
	    setting(lpn_home, LPNHome),
	    atom_concat(LPNHome, URI, Source),
	    setup_call_cleanup(
		http_open(Source, In, [header(content_type, Type)]),
		( format('Content-type: ~w~n~n', [Type]),
		  copy_stream_data(In, current_output)
		),
		close(In))
	).

download(URI, Path) :-
	setting(lpn_home, LPNHome),
	atom_concat(LPNHome, URI, Source),
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
