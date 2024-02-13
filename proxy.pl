:- module(lpn_proxy,
	  [ local_lpn/1,		% +Port
	    server/1			% +Port
	  ]).
:- use_module(convert).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_error)).

/** <module> Learn Prolog Now proxy

This module implements a simple proxy that rewrites LPN to link to SWISH

Overall what this does:

Say there's some HTML page on web
with prolog code examples in it

This program serves that web page on the port specified in server,
at the same relative URI as the source,

so, if the original is at
https://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse1
and we started the proxy by consulting this file and querying
server(4000). then we can see the same page at

http://localhost:4000/lpnpage.php?pagetype=html&pageid=lpn-htmlse1

BUT, this program modifies anything it sees as a Prolog code example in
the original page to be a SWISH console in the process.

Additionally, this program serves a few helper files needed by the
SWISH console.

It makes a lot of assumptions about what the website it's proxying is
like, both in URI structure and in what prolog code will look like.
That's OK, it's just to proxy a single site.

*/

% the location lpn is where files like lpn.css that are needed
% to swish-ize are located
user:file_search_path(lpn, .).
% I'd expect this directory to be in the source tree
% but it's not there.  I've now checked it in.
user:file_search_path(lpn_cache, lpn(cache)).

% by 'location from which we proxy' he means the location
% we get the original, un-swishized html pages from
% this defines a setting named lpn_home, like most settings,
% used for configuration
:- setting(lpn_home, atom,
%	   'https://www.learnprolognow.org',
	   'https://www.let.rug.nl/bos/lpn/',
	   'The location from which we proxy').

% a convenience predicate to override where you get the
% LPN HTML pages from.
local_lpn(Port) :-
	format(atom(URL), 'http://localhost:~w', [Port]),
	set_setting(lpn_home, URL).

% this defines the handlers.
% redirect bare http://localhost:4000/ type request to
% the root of the lpn pages
:- http_handler('/', http_redirect(moved_temporary,
				   '/lpnpage.php?pageid=online'), []).
% this serves all the little extra files like lpn.js, lpn.css etc.
:- http_handler('/', serve_files_in_directory(lpn), [prefix]).
% this is where the meat of the action is, anything that goes to
% lpnpage.php is responded to by the predicate lpn/1
:- http_handler('/lpnpage.php', lpn, []).
% this serves some more of the support structure
% any URI that starts /html/ is served by the predicate pics
:- http_handler('/html/', pics, [prefix]).

server(Port) :-
	http_server(http_dispatch,
		    [ port(Port)
		    ]).

% this is where the fun happens. We SWISH-ize everything served by
%  /lpnpage.php
lpn(Request) :-
	% get the PageID from the request
	option(request_uri(URI), Request),
	pageid(URI, PageID),
	check_file(lpn_cache(PageID)),
	(   absolute_file_name(lpn_cache(PageID), Path,
			       [access(read), file_errors(fail)])
	->  reply_from_file(Path) % if its in the cache SWISHize and send it
	;   absolute_file_name(lpn_cache(PageID), Path,
			       [access(write), file_errors(fail)])
	->  download(URI, Path),  % otherwise download, SWISHize and send it
	    reply_from_file(Path)
	;   setting(lpn_home, LPNHome), % and if we cant cache, SWISHize inline
	    atom_concat(LPNHome, URI, Source), % as it comes from source
	    setup_call_cleanup(
		http_open(Source, In, [connection('Keep-alive')]),
		reply_from_stream(In),
		close(In))
	).

%%	pageid(+URI, -PageID) is semidet
%
%	succeeds binding PageID to the value associated with the pageid
%	key in the query string
%	or fails if thats impossible
%	URI must be an atom, codes, or a string
%
pageid(URI, PageID) :-
	uri_components(URI, Components),
	uri_data(search, Components, Search),
	nonvar(Search),
	uri_query_components(Search, Query),
	memberchk(pageid=PageID, Query).

%%	reply_from_file(+Path:text) is det
%
%	given an abstract file path SWISHize it and
%	send as httpResponse
%
reply_from_file(Path) :-
	setup_call_cleanup(
	    open(Path, read, In),
	    reply_from_stream(In),
	    close(In)).

% Ensure that File is inside ./cache
check_file(File) :-
	absolute_file_name('./cache', Reserved),
	absolute_file_name(File, Tried),
	sub_atom(Tried, 0, _, _, Reserved).

% I think this just proxies the request normal fashion,
% caching as it goes but doesn't swish-ize
pics(Request) :-
	option(path_info(Rest), Request),
	check_file(lpn_cache(Rest)),
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
		http_open(Source, In, [connection('Keep-alive'),header(content_type, Type)]),
		( format('Content-type: ~w~n~n', [Type]),
		  copy_stream_data(In, current_output)
		),
		close(In))
	).

%%	download(+URI:text, +Path:text) is det
%
%	change the source domain for the current URI
%	to lpn_home and download that into the file
%	Path
%
download(URI, Path) :-
	setting(lpn_home, LPNHome),
	atom_concat(LPNHome, URI, Source),
	setup_call_cleanup(
	    http_open(Source, In, [connection('Keep-alive')]),
	    setup_call_cleanup(
		open(Path, write, Out, [type(binary)]),
		copy_stream_data(In, Out),
		close(Out)),
	    close(In)).

%%	reply_from_stream(+In:stream) is det
%
%	read the input stream In, SWISH-ize it,
%	and send as the httpresponse
reply_from_stream(In) :-
	format('Content-type: text/html~n~n'),
	convert_lpn(In, current_output).
