%%  file_atoms(File, Atom) is nondet.
%
%   read each line as atom on backtrack
%
create_list(List) :-
    append([],[], List).

read_lines_from_file(Stream, Lines):-
	read_lines_from_file(Stream, [], Lines).

read_lines_from_file(Stream, In, Out) :-
	read_line_to_codes(Stream, Line),
	(   Line = end_of_file
	->  Out = In
	;   append(In, [Line], NewLine),
	    read_lines_from_file(Stream, NewLine, Out)
	).

read_lines_into_string(Stream, Lines):-
	read_lines_into_string(Stream, "", Lines).

read_lines_into_string(Stream, In, Out):-
	read_line_to_string(Stream, Line),
	%atom_codes(List, Line),
	%string_to_list(List, Line),
	(   Line = end_of_file
	->  Out = In
	;   string_concat(In, Line, NewLine),
	    read_lines_into_string(Stream, NewLine, Out)
	).

display_list([H | T]) :-
	format('~s',[H]), nl,
	write(H), nl,
	display_list(T).

display_list([]).

fix_strings(List, Output):-
	fix_strings(List, "", Output).
fix_strings([H | T], In, Out) :-
	%String = format('~s', [H]),
	string_concat(H, '.', NewString),
	(
	    NewString \= "."
	->  string_concat(In, NewString, Final_String),
	    fix_strings(T, Final_String, Out)
	;   Out = In
	).
file_atoms(File) :-
	setup_call_cleanup(open(File, read, Stream),
			  %read_lines_from_file(Stream, Lines),
			  read_lines_into_string(Stream, Lines),
			  %read_file_to_string(Stream, Lines, ),
			   close(Stream)),
	%display_list(Lines).
	%format('~s', Lines), nl.
	%print(Lines),
	split_string(Lines, '.', "", L),
	fix_strings(L, L1),
	print(L1).
















