testerDCG -->
	"\n[", anything, "].".

anything -->
	[].
anything --> [_], anything.

test(X) :-
	string_to_list(X, X1),
	phrase(testerDCG, X1, []).

