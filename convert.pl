:- module(convert_lpn,
	  [ convert_lpn/2		% +In, +Out
	  ]).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
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
	classify_sources(DOM1),
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
%
%	This preforms three steps:
%
%	  - Extend the head with our dependencies
%	  - Extend the body to call the `swish` jQuery plugin
%	  - Classify sources in `fancyvrb` environments (verbatim)

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
convert(element(div, Attrs0, C0), Source) :-
	select(class=fancyvrb, Attrs0, Attrs),
	(   convert_source(C0, String)
	->  put_attr(Source, 'LPN', source(String, Attrs))
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

back_to_ascii(0x00A0, 0'\s) :- !.		% '
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


%%	classify_sources(+DOM1) is det.
%
%	Classify  the  sources  we  found  on   the  page.  Sources  are
%	attributed variables holding  the  attribute   =LPN=  and  value
%	source(String,  Attributes).  We  want  to  make  the  following
%	inferences:
%
%	  - Which fragments contain source code?
%	  - Which fragments contain queries?
%	  - Indentify relations between source code
%	    - C1 is part of C2
%	      - No need to make C1 executable (highlighted documentation)
%	    - C1 implements the same predicates C2
%	      - Might have do do with alternatives
%	  - Indentify queries
%	    - Query needs only built-ins
%	    - Query needs sources C1, C2, ...

classify_sources(DOM1) :-
	term_attvars(DOM1, Sources),
	maplist(classify_source, Sources).

classify_source(Source) :-
	get_attr(Source, 'LPN', source(String, Attrs)),
	classify_source(String, Content, Class),
	Element = element(pre, [class=Class|Attrs], Content),
	del_attr(Source, 'LPN'),
	Source = Element.


%%	classify_source(C, Class) is det.
%
%	Try to classify the source.

classify_source(C, [C], Class) :-
	source_terms(C, Terms),
	Terms \= [?-_|_], !,
	(   set_source(Terms)
	->  Class = 'swish source'
	;   Class = 'source'			% fragment of previous
	).
classify_source(C, Queries, query) :-
	string_codes(C, Codes),
	phrase(queries(Queries), Codes),
	\+ (Queries = [S], string(S)), !.	% did annotate something.
classify_source(C, [C], code).

queries([Lead, element(span, [class='swish query'], [Query])|More]) -->
	here(Start), string(_), here(SQ),
	"?-", whites, string(S), ".", here(EQ), peek_ws,
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


		 /*******************************
		 *	        XREF		*
		 *******************************/

%%	xref_terms(+Terms, -XRef:dict) is det.
%
%	Cross-reference a list of terms, returning a dict that contains:
%
%	  - defined:ListOfHeads
%	  - called:ListOfHeads
%	  - error:ListOfErrorTerms

xref_terms(Terms, Result) :-
	phrase(xref_terms(Terms), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	dict_pairs(Result, xref, Grouped).

xref_terms([]) --> [].
xref_terms([H|T]) --> xref_term(H), xref_terms(T).

xref_term(Var) -->
	{ var(Var) }, !.
xref_term((Head :- Body)) --> !,
	xref_head(Head),
	xref_body(Body).
xref_term((Head --> Body)) --> !,
	xref_dcg_head(Head),
	xref_dcg_body(Body).
xref_term((:- Body)) --> !,
	xref_body(Body).
xref_term((?- Body)) --> !,
	xref_body(Body).
xref_term(Head) -->
	xref_head(Head).

xref_head(Term) --> { atom(Term) }, !, [defined-Term].
xref_head(Term) --> { compound(Term), !, generalize(Term,Gen) }, [defined-Gen].
xref_head(Term) --> [ error-type_error(callable, Term) ].

xref_body(Term) --> { var(Term) }, !.
xref_body(Term) -->
	{ predicate_property(user:Term, meta_predicate(Meta)), !,
	  generalize(Term, Called),
	  Term =.. [_|Args],
	  Meta =.. [_|Specs]
	},
	[ called-Called ],
	xref_meta(Specs, Args).
xref_body(Term) --> { atom(Term) }, !, [called-Term].
xref_body(Term) --> { compound(Term), !, generalize(Term,Gen) }, [called-Gen].
xref_body(Term) --> [ error-type_error(callable, Term) ].

xref_meta([], []) --> [].
xref_meta([S|ST], [A|AT]) -->
	xref_meta1(S, A),
	xref_meta(ST, AT).

xref_meta1(0, A) --> !,
	xref_body(A).
xref_meta1(^, A0) --> !,
	{ strip_existential(A0, A) },
	xref_body(A).
xref_meta1(N, A0) -->
	{ integer(N), N > 0, !,
	  extend(A0, N, A)
	},
	xref_body(A).
xref_meta1(_, _) --> [].


xref_dcg_head(Var) -->
	{ var(Var) }, !,
	[ error-instantiation_error(Var) ].
xref_dcg_head((A,B)) -->
	{ is_list(B) }, !,
	xref_dcg_head(A).
xref_dcg_head(Term) -->
	{ atom(Term), !,
	  functor(Head, Term, 2)
	},
	[ defined-Head ].
xref_dcg_head(Term) -->
	{ compound(Term), !,
	  compound_name_arity(Term, Name, Arity0),
	  Arity is Arity0+2,
	  compound_name_arity(Gen, Name, Arity)
	},
	[ defined-Gen ].
xref_dcg_head(Term) -->
	[ error-type_error(callable, Term) ].

xref_dcg_body(Body) -->
	{ var(Body) }, !.
xref_dcg_body(Body) -->
	{ dcg_control(Body, Called) }, !,
	xref_dcg_body_list(Called).
xref_dcg_body(Terminal) -->
	{ is_list(Terminal) ; string(Terminal) }, !.
xref_dcg_body(Term) -->
	{ atom(Term), !,
	  functor(Head, Term, 2)
	},
	[ called-Head ].
xref_dcg_body(Term) -->
	{ compound(Term), !,
	  compound_name_arity(Term, Name, Arity0),
	  Arity is Arity0+2,
	  compound_name_arity(Gen, Name, Arity)
	},
	[ called-Gen ].
xref_dcg_body(Term) -->
	[ error-type_error(callable, Term) ].

dcg_control((A,B), [A,B]).
dcg_control((A;B), [A,B]).
dcg_control((A->B), [A,B]).
dcg_control((A*->B), [A,B]).
dcg_control(\+(A), [A]).

xref_dcg_body_list([]) --> [].
xref_dcg_body_list([H|T]) --> xref_dcg_body(H), xref_dcg_body_list(T).

strip_existential(T0, T) :-
	(   var(T0)
	->  T = T0
	;   T0 = _^T1
	->  strip_existential(T1, T)
	;   T = T0
	).

extend(T0, N, T) :-
	atom(T0), !,
	length(Args, N),
	T =.. [T0|Args].
extend(T0, N, T) :-
	compound(T0),
	compound_name_arguments(T0, Name, Args0),
	length(Extra, N),
	append(Args0, Extra, Args),
	compound_name_arguments(T, Name, Args).

generalize(Compound, Gen) :-
	compound_name_arity(Compound, Name, Arity),
	compound_name_arity(Gen, Name, Arity).

