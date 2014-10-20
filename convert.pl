:- module(convert_lpn,
	  [ convert_lpn/2		% +In, +Out
	  ]).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).

:- debug(lpn).

convert_lpn(In, Out) :-
	load_html(In, DOM, []),
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

convert(element(head, Args, C0),
	element(head, Args,
		[ element(meta, [charset='UTF-8'], [])
		| C
		])) :- !,
	convert_dom(C0, C).
convert(element(div, Attrs0, C0),
	element(pre, [class=Class|Attrs], [C])) :-
	select(class=fancyvrb, Attrs0, Attrs),
	(   convert_source(C0, C, Class)
	->  true
	;   debug(lpn, 'Failed to convert ~p', [C0]),
	    fail
	).

convert_source(Content, Text, source) :-
	phrase(content_text(Content), Fragments),
	atomics_to_string(Fragments, Text).

content_text(CDATA) -->
	{ atom(CDATA), !,
	  split_string(CDATA, " \t\n", " \t\n", Parts),
	  atomics_to_string(Parts, " ", Text)
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
