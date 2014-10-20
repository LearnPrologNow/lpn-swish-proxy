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
	load_html(In, DOM,
		  [ syntax_errors(quiet),
		    max_errors(-1)
		  ]),
	convert_dom(DOM, DOM1),
	setup_call_cleanup(
	    open(Out, write, Stream),
	    html_write(Stream, DOM1, []),
	    close(Stream)).

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

convert(element(script, Args, [C0]),
	element(script, Args, [C])) :-
	string_concat("<!--", C1, C0),
	string_concat(C2, "-->", C1), !,
	string_concat(C2, "\n", C).
convert(element(head, Args, C0),
	element(head, Args,
		[ element(meta, [charset='UTF-8'], []),
		  element(link, [href='lpn.css', rel=stylesheet], []),
		  element(script, [src='jquery.min.js'], []),
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
$("body").LPN();
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
	  maplist(nbsp, Codes0, Codes),
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

nbsp(0x00A0, 32) :- !.
nbsp(X, X).

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

classify_source(C, [C], source) :-
	source_terms(C, Terms),
	Terms \= [?-_|_], !.
classify_source(C, ["?- ", element(span, [class=query], [QT]), AC], query) :-
	string_codes(C, Codes),
	phrase((whites, "?-", whites, string(S), "."), Codes, Rest),
	string_codes(QT0, S),
	catch(term_string(_T, QT0), _, fail), !,
	string_concat(QT0, ".", QT),
	string_codes(AC, Rest).

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
