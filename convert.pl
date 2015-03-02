:- module(convert_lpn,
	  [ convert_lpn/2,		% +In, +Out
	    local_swish/1		% +Port
	  ]).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(dcg/basics)).
:- use_module(library(settings)).
:- use_module(library(ordsets)).
:- use_module(library(xpath)).

:- setting(swish, atom,
	   'http://swish.swi-prolog.org/',
	   'SWISH server url').

local_swish(Port) :-
	format(atom(SWISH), 'http://localhost:~w/', [Port]),
	set_setting(swish, SWISH).

:- debug(lpn(convert)).
:- debug(lpn(_)).

%%	convert_lpn(+In:Stream, +Out:stream) is det
%
% SWISHize In as it's copied to Out
%
convert_lpn(In, Out) :-
	% the \+ \+ enforces determinacy
	\+ \+ convert_lpn2(In, Out).

%%	convert_lpn2(+In:Stream, +Out:stream) is nondet
%
% SWISHize In as it's copied to Out
%
% Method used is to read in via load_html, which creates
% a tree structure DOM, convert parts of the DOM that
% are to be SWISHized - @see convert_dom/2 and convert/2
%




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


%%       list_to_strings(+List:List, +Output: String) is det
%
%        This method takes in LPN Prolog code as List as an
%        input aragument and converts it into a String

list_to_strings(List, Output):-
	list_to_strings(List, "", Output).



list_to_strings([H | T], In, Out) :-
	string_concat(H, '.', NewString),
	(
	    % This is a corner case to detect an empty string
	    NewString \= "."
	->  string_concat(In, NewString, Final_String),
	    list_to_strings(T, Final_String, Out)
	;   list_to_strings(T, In, Out)
	).

list_to_strings([], In, Out) :-
	Out = In.

%%	sort_lpn_codes(+Input: String, Output:String) is det
%	This method takes in LPN code as an input argument
%	and returns an Output String which is sorted.
%	This has been done to fix SWISH errors like:
%	Clauses of <wizard>/1 are not together in the source-file
%
%


sort_lpn_codes(Input, Output) :-
	% Fix for the corner case where the first predicate
	% in the source code is missing a new line character
	string_concat('\n', Input, InputString),
	split_string(InputString, '.', '', SplitList),
	sort(SplitList, NewList),
	list_to_strings(NewList, Output).

%%	convert_dom(+DOM0, -DOM) is semidet
%
%	convert a DOM or sub-DOM
%	if convert/2 can convert it, then it's a
%	structure to be modified. If not, we recursively
%	try to convert

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
%
%	  'fancyvrb' is a class in the original sources

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
	setting(swish, SWISH),
	format(string(Script),
	       '$(function() { $(".swish").LPN({swish:"~w"}); });', [SWISH]),
	append(C1,
	       [ element(script, [], [Script])
	       ], C).
convert(element(ol, Attrs0, C0),
	element(ol, Attrs, C1)) :-
	query_list(C0), !,
	convert_dom(C0, C1),
	dom_add_class(Attrs0, 'swish query-list', Attrs).
convert(element(div, Attrs, []),
	element(div, Attrs, C)) :-
	memberchk(class=coloredbar, Attrs), !,
	C = {|html||
<div class="swish-disclaimer">
This version of Learn Prolog Now! embeds <a href="http://swish.swi-prolog.org">
<span style="color:darkblue">SWI</span><span style="color:maroon">SH</span></a>,
<a href="http://www.swi-prolog.org">SWI-Prolog</a> for SHaring.
The current version rewrites the Learn Prolog Now! HTML on the fly, recognising
source code and example queries.  It is not yet good at recognising the relations
between source code fragments and queries.  Also Learn Prolog Now! needs some
updating to be more compatible with SWI-Prolog.  All sources are on GitHub:

<div class="github">
<span>LearnPrologNow</span>

<iframe class="github-btn" src="http://ghbtns.com/github-btn.html?user=LearnPrologNow&amp;repo=lpn&amp;type=fork&amp;count=true" width="102" height="20" title="Fork on GitHub"></iframe>

<span>LPN SWISH Proxy</span>

<iframe class="github-btn" src="http://ghbtns.com/github-btn.html?user=LearnPrologNow&amp;repo=lpn-swish-proxy&amp;type=fork&amp;count=true" width="102" height="20" title="Fork on GitHub"></iframe>

<span>SWISH</span>

<iframe class="github-btn" src="http://ghbtns.com/github-btn.html?user=SWI-Prolog&amp;repo=swish&amp;type=fork&amp;count=true" width="102" height="20" title="Fork on GitHub"></iframe>
</div>
</div>
	    |}.


convert(element(div, Attrs0, C0),
	element(div, Attrs, C1)) :-
	memberchk(class=newtheorem, Attrs0),
	xpath_chk(element(div, Attrs0, C0), p/span, Span),
	xpath_chk(Span, //strong(text), 'Exercise'), !,
	convert_dom(C0, C1),
	dom_add_class(Attrs0, 'swish exercise', Attrs).
convert(element(div, Attrs0, C0), Source) :-
	select(class=fancyvrb, Attrs0, Attrs),
	(   convert_source(C0, String),
	    sort_lpn_codes(String, String1)
	->  put_attr(Source, 'LPN',
		     verb{text:String1,
			  attributes:Attrs,
			  element:Source})
	;   debug(lpn(convert), 'Failed to convert ~p', [C0]),
	    fail
	).

dom_add_class(Attrs0, Class, Attrs) :-
	select(class=Class0, Attrs0, Attrs1), !,
	atomic_list_concat([Class0, Class], ' ', NewClass),
	Attrs = [class=NewClass|Attrs1].
dom_add_class(Attrs0, Class, [class=Class|Attrs0]).

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
%	attributed variables holding the  attribute   =LPN=  and value a
%	dict. We want to make the following inferences:
%
%	  - Which fragments contain source code?
%	    - Valid Prolog terms without "?-"
%	  - Which fragments contain queries?
%	    - One or more "?- Term." sequences
%	  - Indentify relations between source code
%	    - S1 follows S2 and is part of S2
%	      - No need to make C1 executable (highlighted documentation)
%	    - S1 implements the same predicates S2
%	      - Might have do do with alternatives
%	    - S1 requires S2.
%	  - Indentify queries
%	    - Query needs only built-ins
%	    - Query needs sources S1, S2, ...
classify_sources(DOM) :-
	term_attvars(DOM, Vars),
	pre_classify_verbs(Vars),
	id_source_highlights(Vars),
	id_queries(Vars),
	id_variants(Vars),
%	id_depends(Vars),
	maplist(bind, Vars).

pre_classify_verbs([]).
pre_classify_verbs([V|T0]) :-
	get_lpn(V, H),
	pre_classify_source(H.text, Content, Terms, Class),
	xref_terms(Terms, XREF),
	H1 = H.put(content, Content),
	(   Class == verbatim
	->  bind(H1, [class=verbatim])
	;   set_lpn(V, H1.put(_{terms:Terms, xref:XREF, class:Class}))
	),
	pre_classify_verbs(T0).

get_lpn(Var, Dict) :- get_attr(Var, 'LPN', Dict).
set_lpn(Var, Dict) :- put_attr(Var, 'LPN', Dict).

put_lpn(Var, New) :-
	get_lpn(Var, Dict0),
	set_lpn(Var, Dict0.put(New)).
put_lpn(Var, Key, Value) :-
	get_lpn(Var, Dict0),
	set_lpn(Var, Dict0.put(Key, Value)).

bind(Var) :-
	get_lpn(Var, Dict), !,
	final_class(Dict.class, Class),
	bind(Dict, [class=Class]).
bind(_).

final_class(source, 'source swish') :- !.
final_class(Class, Class).

bind(Dict, Attrs) :-
	del_attr(Dict.element, 'LPN'),
	append(Attrs, Dict.attributes, AllAttrs),
	Dict.element = element(pre, AllAttrs, Dict.content).

has_class(Class, V) :-
	get_lpn(V, Dict),
	Class == Dict.class.

add_class(V, Class) :-
	get_lpn(V, Dict),
	atomic_list_concat([Dict.class, Class], ' ', NewClass),
	set_lpn(V, Dict.put(class, NewClass)).

set_class(V, Class) :-
	get_lpn(V, Dict),
	set_lpn(V, Dict.put(class, Class)).

elem_id(Elem, ID) :-				% add an id if there is none?
	get_lpn(Elem, Dict),
	memberchk(id=ID, Dict.attributes).

add_attr(V, Name, Value) :-
	get_lpn(V, Dict),
	set_lpn(V, Dict.put(attributes, [Name=Value|Dict.attributes])).

%%	id_source_highlights(+Vars)
%
%	Identify source fragments that are highlights of previous
%	source fragments.  Such fragments form a sub-list.

id_source_highlights(Vars) :-
	source_highlights(Vars, -).

source_highlights([], _).
source_highlights([H|T], C) :-
	has_class(source, H), !,
	(   C == (-)
	->  source_highlights(T, H)
	;   highlight_of(H, C)
	->  add_class(H, highlight),
	    source_highlights(T, C)
	;   source_highlights(T, H)
	).
source_highlights([_|T], C) :-
	source_highlights(T, C).

%%	highlight_of(+Highlight, +Source)
%
%	True if Highlight is a consequtive  sublist of Source. Note that
%	we must use a variant check for   the  sublist rather than plain
%	unification.

highlight_of(H, S) :-
	get_lpn(H, HD),
	get_lpn(S, SD),
	sub_list(HD.terms, SD.terms),
	debug(lpn(highlight), '~w is highlight of ~w', [HD.text, SD.text]).

sub_list(Sub, List) :-
	length(Sub, Len),
	length(Sub2, Len),
	append(Sub2, _, SubV),
	append(_, SubV, List),
	Sub =@= Sub2, !.

%%	id_queries(+Vars) is det.
%
%	Classify source fragments that are in  fact queries. This is the
%	case if they consists of only a single term and this term can be
%	executed in the context of preceeding (?) sources.

id_queries(Vars) :-
	id_queries(Vars, []).

id_queries([], _).
id_queries([H|T], SL) :-
	(   has_class(source, H)
	->  true
	;   has_class(maybe_query, H)
	),
	get_lpn(H, Dict),
	Dict.terms = [Query],
	xref_terms([?-Query], xref{called:Called}),
	can_run_in(Called, SL, _Used), !,
	make_query(H, Called),
	id_queries(T, SL).
id_queries([H|T], SL) :-
	has_class(source, H), !,
	id_queries(T, [H|SL]).
id_queries([H|T], SL) :-
	has_class(maybe_query, H), !,
	set_class(H, verbatim),
	bind(H),
	id_queries(T, [H|SL]).
id_queries([_|T], SL) :-
	id_queries(T, SL).

make_query(V, Called) :-
	get_lpn(V, Dict),
	debug(lpn(query), 'Source seems query: ~w', [Dict.text]),
	[Query] = Dict.terms,
	string_concat("?- ", Dict.text, Text0),
	ensure_fullstop(Text0, Text),
	Content = ["", element(span, [class='swish query guessed'], [Text])],
	set_lpn(V, Dict.put(_{content:Content,
			      text:Text,
			      class:query,
			      terms:[?-Query],
			      xref:xref{called:Called}
			     })).

ensure_fullstop(Text, Text) :-
	split_string(Text, "", " \t\n", [Trimmed]),
	sub_string(Trimmed, _, _, 0, "."), !.
ensure_fullstop(Text0, Text) :-
	string_concat(Text0, ".", Text).

%%	can_run_in(+Called, +Sources, -UseSources) is semidet.
%
%	True if Called can run with Sources, where UseSources is the
%	subset of Sources that is actually used.

can_run_in(Called, Sources, Used) :-
	exclude(built_in, Called, Required),
	include(references(Required), Sources, Used),
	maplist(provides, Used, ProvidesSuper),
	append(ProvidesSuper, Provides),
	sort(Provides, ProvidesSet),
	ord_subset(Required, ProvidesSet).

built_in(Name/Arity) :-
	functor(Head, Name, Arity),
	predicate_property(Head, built_in).

references(Required, Source) :-
	provides(Source, Provides),
	ord_intersect(Required, Provides).

provides(Source, Provides) :-
	get_lpn(Source, Dict),
	(   Defined = Dict.xref.get(defined)
	->  Provides = Defined
	;   Provides = []
	).

%%	id_variants(+Vars)
%
%	Identify that one source fragment is  the variant of another. We
%	want to test that the source fragments   provide the same set of
%	predicates. That is basically Defined\Called,  but the status of
%	recursive predicates is unclear.  Possibly   we  should evaluate
%	that in the context of the queries?
%
%	Found variants are indicated in two ways:
%
%	  - They get an HTML attribute 'data-variant-id' with a unique
%	    id.
%	  - The dict gets a key =variants=, which is a dict with keys
%	    =before= and =after=

id_variants(Vars) :-
	include(has_class(source), Vars, Sources),
	map_list_to_pairs(exports, Sources, ExportTagged),
	group_pairs_by_key(ExportTagged, ByExport),
	tag_variants(ByExport, 0).

exports(Source, ExportPIs) :-
	get_lpn(Source, Dict),
	XREF = Dict.xref,
	(   exclude(built_in, XREF.get(called), Required)
	->  true
	;   Required = []
	),
	(   Defined = XREF.get(defined)
	->  true
	;   Defined = []
	),
	ord_intersection(Required, Defined, Recursive),
	ord_subtract(Defined, Required, NeededPIs),
	ord_union(Recursive, NeededPIs, ExportPIs).

tag_variants([], _).
tag_variants([_-[]|T], Id) :- !,
	tag_variants(T, Id).
tag_variants([_-Group|T], Id0) :-
	succ(Id0, Id),
	atom_concat('group-', Id, GroupID),
	debug(lpn(variant), 'Variant group ~q: ~p', [GroupID, Group]),
	tag_variants(Group, [], GroupID),
	tag_variants(T, Id).

tag_variants([], _, _).
tag_variants([H|T], Before, ID) :-
	add_attr(H, 'data-variant-id', ID),
	put_lpn(H, variants, variants{before:Before,after:T}),
	tag_variants(T, [H|Before], ID).

head_pi(Head, Head/0) :-
	atom(Head), !.
head_pi(Head, Name/Arity) :-
	compound_name_arity(Head, Name, Arity).

%%	id_depends(+Vars)
%
%	Find source dependencies.  A source depends on another if it
%	requires predicates that are provided by another.
%
%	note: I suspect Jan never got this working - Annie
%

id_depends(Vars) :-
	include(has_class(source), Vars, Sources),
	compound_name_arguments(Array, a, Sources),
	findall(dep(Depends,On,Preds),
		depends(Array, Depends, On, Preds), __Triples).

depends(Array, Depends, On, Preds) :-
	arg(Depends, Array, VD),
	arg(On, Array, VO),
	On \== Depends,
	get_lpn(VD, DependsData),
	get_lpn(VO, OnData),
	ord_intersection(DependsData.xref.get(required),
			 OnData.xref.get(defined), Preds),
	Preds \== [].


%%	pre_classify_source(+String, -Content, -Terms, -Class) is det.
%
%	Pre classification of  a  source  fragment.   Class  is  one  of
%	=source=, =query= or =verbatim=. Terms is a list of Prolog terms
%	found.

pre_classify_source(C, [C], Terms, source) :-
	source_terms(C, Terms),
	Terms \= [?-_|_],
	xref_terms(Terms, XREF),
	\+ ( member(Defined, XREF.get(defined)),
	     forbidden_defined(Defined)
	   ), !.
pre_classify_source(C, Queries, Terms, query) :-
	string_codes(C, Codes),
	phrase(queries(Queries, Terms), Codes),
	\+ (Queries = [S], string(S)), !.	% did annotate something.
pre_classify_source(C, [C], [Term], maybe_query) :-
	catch(term_string(Term, C), _, fail),
	\+ noquery(Term).
pre_classify_source(C, [C], [], verbatim).

forbidden_defined('/'/2).
forbidden_defined('='/2).

%%	noquery(+Term)
%
%	Term is not a sensible query.

noquery(Var) :- var(Var), !.
noquery([_|_]) :- !, fail.
noquery(NotCallable) :- \+ callable(NotCallable), !, fail.
noquery(X) :- answer_term(X), !.
noquery(Name/Arity) :- atom(Name), integer(Arity).

answer_term(Var) :- var(Var), !, fail.
answer_term(Var = _Value) :- var(Var), !.
answer_term((A;B)) :- answer_term(A), answer_term(B).


queries([Lead, element(span, [class='swish query'], [Query])|More],
        [(?- Term)|Terms]) -->
	here(Start), string(_), here(SQ),
	"?-", whites, string(S), ".", here(EQ), peek_ws,
	{ string_codes(QS, S),
	  catch(term_string(Term, QS), _, fail), !,
	  string_between(Start, SQ, Lead),
	  string_between(SQ, EQ, Query)
	},
	queries(More, Terms).
queries([Rest], []) -->
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

%%	query_list(+ContentList) is semidet.
%
%	True if ContentList is a <li> list which consist of mostly
%	query-like terms.

query_list(ContentList) :-
	partition(query_element, ContentList, Queries, NonQueries),
	length(Queries, QLen),
	length(NonQueries, NQLen),
	QLen >= NQLen.

query_element(Element) :-
	convert_source(Element, String),
	catch(term_string(Term, String), _, fail),
	\+ noquery(Term).


		 /*******************************
		 *	        XREF		*
		 *******************************/

%%	xref_terms(+Terms, -XRef:dict) is det.
%
%	Cross-reference a list of terms, returning a dict that contains:
%
%	  - defined:OrdSetOfPI
%	  - called:OrdSetOfPI
%	  - required:OrdSetOfPI
%	  - error:SetOfErrorTerms
%
%	Note that XRef.required is XRef.called \ built-in \XRef.defined.

xref_terms(Terms, Result) :-
	phrase(xref_terms(Terms), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	maplist(value_to_set, Grouped, GroupedSets),
	dict_pairs(Result0, xref, GroupedSets),
	(   exclude(built_in, Result0.get(called), Called),
	    ord_subtract(Called, Result0.get(defined), Required),
	    Required \== []
	->  Result = Result0.put(required, Required)
	;   Result = Result0
	).

value_to_set(error-List, error-Set) :- !,
	variant_set(List, Set).
value_to_set(Key-HeadList, Key-PISet) :-
	maplist(head_pi, HeadList, PIList),
	sort(PIList, PISet).

variant_set(List, Set) :-
	list_to_set(List, Set1),
	remove_variants(Set1, Set).

remove_variants([], []).
remove_variants([H|T0], [H|T]) :-
	skip_variants(T0, H, T1),
	remove_variants(T1, T).

skip_variants([H|T0], V, T) :-
	H =@= V, !,
	skip_variants(T0, V, T).
skip_variants(L, _, L).


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

