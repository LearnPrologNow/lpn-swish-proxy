:- module(convert_lpn,
	  [ convert_lpn/2		% +In, +Out
	  ]).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

:- debug(lpn).

convert_lpn(In, Out) :-
	\+ \+ convert_lpn2(In, Out).

convert_lpn2(In, Out) :-
	load_html(In, DOM,
		  [ syntax_errors(quiet),
		    max_errors(-1)
		  ]),
	convert_dom(DOM, DOM1),
	(   is_stream(Out)
	->  html_write(Out, DOM1, [])
	;   setup_call_cleanup(
		open(Out, write, Stream),
		html_write(Stream, DOM1, []),
		close(Stream))
	).

convert_dom(DOM0, DOM) :-
	convert(DOM0, DOM), !.
convert_dom(CDATA0, CDATA) :-
	atom(CDATA0), !,
	CDATA = CDATA0.
convert_dom([], []).
convert_dom([H0|T0], [H|T]) :-
	convert_dom(H0, H),
	convert_dom(T0, T).
convert_dom(element(E,A,C0), element(E,A,C)) :-
	convert_dom(C0,C).

%%	convert(+DOM0, -DOM) is semidet.

convert(element(head, Args, C0),
	element(head, Args,
		[ element(meta, [charset='UTF-8'], []),
		  element(link, [href='lpn.css', rel=stylesheet], []),
		  element(link, [href='jquery-ui.min.css', rel=stylesheet], []),
		  element(script, [src='jquery.min.js'], []),
		  element(script, [src='jquery-ui.min.js'], []),
		  element(script, [src='lpn.js'], [])
		| C
		])) :- !,
	convert_dom(C0, C).
convert(element(body, Args, C0),
	element(body, Args, C)) :- !,
	convert_dom(C0, C1),
	append(C1,
	       [ element(script, [], [
'$(function() {
$(".swish").LPN();
});
'])
	       ], C).
convert(element(div, Attrs0, C0),
	element(pre, [class=Class|Attrs], Pre)) :-
	select(class=fancyvrb, Attrs0, Attrs),
	(   convert_source(C0, C)
	->  classify_source(C, Pre, Class)
	;   debug(lpn, 'Failed to convert ~p', [C0]),
	    fail
	).

%%	convert_source(+Content, -String) is det.
%
%	Get the source text from the `fancyvrb` div

convert_source(Content, Text) :-
	phrase(content_text(Content), Fragments),
	atomics_to_string(Fragments, Text0),
	remove_leading_spaces(Text0, Text).

content_text(CDATA) -->
	{ atom(CDATA), !,
	  split_string(CDATA, " \t\n", " \t\n", Parts),
	  atomics_to_string(Parts, " ", Text0),
	  string_codes(Text0, Codes0),
	  maplist(back_to_ascii, Codes0, Codes),
	  string_codes(Text, Codes)
	}, !,
	[ Text ].
content_text([]) --> [].
content_text([H|T]) -->
	content_text(H),
	content_text(T).
content_text(element(br, _, _)) --> !,
	['\n'].
content_text(element(_, _, Content)) -->
	content_text(Content).

back_to_ascii(0x00A0, 0'\s) :- !.
back_to_ascii(0x2019, 0'\') :- !.		% Unicode right single quote
back_to_ascii(X, X).

%%	remove_leading_spaces(+In, -Out)

remove_leading_spaces(In, Out) :-
	split_string(In, "\n", "", Lines),
	maplist(leading_spaces, Lines, LeadingPerLine),
	min_list(LeadingPerLine, Leading),
	length(List, Leading),
	maplist(=(32), List),
	string_codes(LeadingStr, List),
	maplist(string_concat(LeadingStr), Stripped, Lines), !,
	atomics_to_string(Stripped, "\n", Out).
remove_leading_spaces(In, In).

leading_spaces(Line, Count) :-
	leading_spaces(Line, 0, Count).

leading_spaces(Line, N, Count) :-
	sub_string(Line, N, 1, _, " "), !,
	N2 is N+1,
	leading_spaces(Line, N2, Count).
leading_spaces(_, N, N).

%%	classify_source(C, Class) is det.
%
%	Try to classify the source.

classify_source(C, [C], Class) :-
	source_terms(C, Terms),
	Terms \= [?-_|_], !,
	(   set_source(Terms)
	->  Class = 'swish source'
	;   Class = nosource
	).
classify_source(C, Queries, query) :-
	string_codes(C, Codes),
	phrase(queries(Queries), Codes).

queries([Lead, element(span, [class='swish query'], [Query])|More]) -->
	here(Start), string(_), here(SQ), "?-", whites, string(S), ".", here(EQ),
	peek_ws,
	{ string_codes(QS, S),
	  catch(term_string(_T, QS), _, fail), !,
	  string_between(Start, SQ, Lead),
	  string_between(SQ, EQ, Query)
	},
	queries(More).
queries([Rest]) -->
	string(Codes), \+ [_], !,
	{ string_codes(Rest, Codes) }.

peek_ws, [C] --> [C], {ws(C)}, !.
peek_ws --> \+ [_].

ws(0'\s).
ws(0'\t).
ws(0'\n).

here(T,T,T).

string_between(Start, End, String) :-
	diff_codes(Start, End, Codes),
	string_codes(String, Codes).

diff_codes(Start, End, Empty) :-
	Start == End, !,
	Empty = [].
diff_codes([H|T0], End, [H|T]) :-
	diff_codes(T0, End, T).


%%	set_source(+Terms) is semidet.
%
%	Set our notion of the  current  source.   If  we  find  that the
%	current element provides a sub-sequence   of the current source,
%	we assume it is a description that  highlights a fragment and do
%	not update our notion of the current source.

set_source(Terms) :-
	nb_current(lpn_source, Source),
	append(Terms, _, OpenTerms),
	append(_, OpenTerms, Source), !,
	fail.
set_source(Terms) :-
	b_setval(lpn_source, Terms).


%%	source_terms(+Text, -Terms) is semidet.
%
%	True when Terms is a list of terms represented in Text. Fails if
%	there are syntax errors.

source_terms(Text, Terms) :-
	catch(setup_call_cleanup(
		  open_codes_stream(Text, Stream),
		  read_stream_to_terms(Stream, Terms),
		  close(Stream)),
	      error(syntax_error(_), _),
	      fail).

read_stream_to_terms(Stream, Terms) :-
	read(Stream, T0),
	read_stream_to_terms(T0, Stream, Terms).

read_stream_to_terms(Term, _, []) :-
	Term == end_of_file, !.
read_stream_to_terms(Term, Stream, [Term|Rest]) :-
	read(Stream, T0),
	read_stream_to_terms(T0, Stream, Rest).
