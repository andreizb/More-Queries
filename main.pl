
/**
  * Varianta curenta nu foloseste predicate de sortare din Prolog.
  * Deoarece am apucat sa incarc o varianta care folosea predsort pana la
  * mentionarea pe forum a acestei restrictii, voi include aici partile de
  * cod, comentate, care erau diferite in cadrul complex_query2, pentru a
  * nu se considera vreo neregula.
  *
  * Prin comentariul de pe forum am vrut doar sa atrag atentia ca in lipsa
  * intrebarii pe forum si/sau lipsa verificarii forumului dupa trimiterea
  * temei, m-as fi ales cu depunctare fara sa inteleg motivul.
  *
  * comparator(<, [A, _], [B, _]) :- A < B.
  * comparator(>, _, _).
  *
  * eval(complex_query2(G, L, U), R) :- eval(tfilter([_, _, Gen, Rtg],
  * ((string_concat(K, _, Gen), string_concat(_, G, K), !), Rtg >= L, Rtg =< U),
  *		join(append, ["movie_id","title","genres", "rating"], table(movies),
  *					select(["rating"], aux_query(table(ratings))))), R), !.
  *
  * eval(aux_query(Q), [H|R]) :- eval(select(["movie_id", "rating"],
  *						Q), [H|Rp]), predsort(comparator, Rp, R). 
  */

% Include modulele cu tabele si predicate folosite de catre checker.
:- use_module(tables).
:- use_module(check_predicates).


% Predicate auxiliare folosite pentru formatarea tabelului.
plus5(X, Y):- Y is X + 5.
make_format_str(MaxRowLen, Str) :-
							maplist(plus5, MaxRowLen, Rp), aux_format(Rp, Str).
aux_format([H], R) :- string_concat("~t~w~t~", H, R1),
												string_concat(R1, "+~n", R), !.
aux_format([H|T], R) :- string_concat("~t~w~t~", H, R1),
	string_concat(R1, "+ ", R2), aux_format(T, Rp), string_concat(R2, Rp, R).


/**
  * Predicatul care printeaza un tabel. Se porneste de la o lista cu valori 0,
  * cu o lungime egala cu cea a numarului de coloane din tabel. Se va determina
  * prin parcurgerea tabelului rand cu rand, sirul de lungime maxima gasit pe
  * coloana cu indicele corespunzator in lista formata.
  */
print_table_op(Tbl) :- table_columns(Tbl, Len), build(0, Len, Start), 
		table_max_len(Tbl, Start, MaxRowLen), make_format_str(MaxRowLen, R),
														print_rows(Tbl, R).


/**
  * Predicat auxiliar, folosit pentru a afisa tabelul. Se va satisface intai
  * predicatul format, apoi se va incerca satisfacerea recursiva.
  */
print_rows([], _).
print_rows([H|T], R) :- format(R, H), print_rows(T, R).


% Predicat auxiliar, determina numarul de coloane din tabel.
table_columns([H|_], Len) :- length(H, Len).


/**
  * Predicat auxiliar, care primeste un rand din tabel si o lista. Daca lungimea
  * unei coloane din randul de tabel primit este mai mare decat valoarea de la
  * indexul corespunzator, valoarea mai mica va fi inlocuita.
  */
get_max_len(_, [], []). 
get_max_len([H|T], [X|XS], [Len|R]) :- string_length(H, Len), Len > X,
													get_max_len(T, XS, R), !.
get_max_len([_|T], [X|XS], [X|R]) :- get_max_len(T, XS, R), !.


/**
  * Predicat auxiliar, care primeste un tabel si o lista si determina lista care
  * va avea la fiecare index corespunzator, lungimea maxima de pe acea coloana.
  */
table_max_len([H], R, Res) :- get_max_len(H, R, Res).
table_max_len([H|T], R, Res) :- table_max_len(T, R, Resp),
												get_max_len(H, Resp, Res), !.


% Predicat auxiliar, care va construi o lista de lungime N, cu toate valorile X.
build(_, 0, []).
build(X, N, List)  :- length(List, N), maplist(=(X), List), !.


% Predicatul tail, care primeste o lista si intoarce lista fara primul element.
tail([], []).
tail([_|T], T).


% Predicatul head, care primeste o lista si returneaza primul element din lista.
head([], _) :- fail.
head([A|_], A).

/**
  * Predicatul care primeste o matrice si returneaza transpusa.
  * Aplicand maplist cu predicatul head, vom construi o lista care contine
  * primele elemente de pe fiecare linie, cu alte cuvinte prima coloana.
  * Aplicand apoi maplist cu predicatul tail, vom avea matricea fara primele
  * elemente de pe fiecare linie, asupra careia vom aplica predicatul
  * de transpunere.
  * Astfel, vom obtine o lista cu coloanele, in ordine, cu alte cuvinte exact
  * transpusa matricei.
  */
trans([[]|_], []).
trans(Tbl, [Hp|R]) :- maplist(head, Tbl, Hp), maplist(tail, Tbl, Tblp),
														trans(Tblp, R), !.


% Predicat auxiliar, care realizeaza suma elementelor dintr-o lista.
sum([], 0).
sum([H|T], Sp):- sum(T, S), Sp is S + H.


/**
  * Predicat auxiliar, folosit de catre complex_q2.
  * Este satisfacut daca in cadrul liniei curente din al doilea tabel,
  * movie_id-ul este acelasi cu al liniei curente din primul tabel sau
  * daca acest movie_id este gasit ulterior, dintr-o cautare recursiva.
  * Stiind ca avem acelasi numar de linii cu aceleasi movie_id-uri, doar
  * in ordini diferite, trebuie sa ii putem asocia fiecarei coloane din
  * primul tabel un rating.
  */
get_equal([], _, []).
get_equal([A, _, _], [[B, C]|_], [B, C]) :- A == B.
get_equal(A, [_|T], R) :- get_equal(A, T, R).


/** 
  * Predicat folosit pentru a ordona intrarile din al doilea tabel, care
  * contine coloane de movie_id si rating, astfel incat sa corespunda
  * ordinii movie_id-urilor din primul tabel.
  */
complex_q2([], _, []).
complex_q2([H|T], Tbl2, [Res|R]) :- get_equal(H, Tbl2, Res),
													complex_q2(T, Tbl2, R).


/**
  * Predicat care primeste doua tabele, dintre care al doilea cu exact doua
  * coloane si concateneaza cea de-a doua coloana din al doilea tabel
  * la primul tabel.
  */
append_q2([], _, []).
append_q2([H1|T1], [[_, B]|T2], [C|R]) :- append(H1, [B], C),
														append_q2(T1, T2, R).


/**
  * Predicat folosit in cadrul query-ului "select". Primeste tabelul transpus,
  * care va fi prin urmare o lista de coloane, cu primul element denumirea
  * coloanei. Daca denumirea este prezenta in lista de coloane ce trebuie
  * selectate, vom include aceasta "coloana" in tabelul transpus rezultat.
  */ 
select_op(_, [], _).
select_op([[H|T]|Tp], [X|XS], [[H|T]|R]) :- H == X, select_op(Tp, XS, R).
select_op([_|T], XS, R) :- select_op(T, XS, R).


/**
  * Predicat auxiliar folosit de catre "join_op". Primeste un predicat, doua
  * tabele si returneaza tabelul rezultat. Predicatul primit trebuie sa
  * primeasca randul curent din fiecare tabel si sa intoarca randul din tabelul
  * rezultat.
  */
aux_join(_, [], _, []).
aux_join(Op, [H1|T1], [H2|T2], [Hr|R]) :- call(Op, H1, H2, Hr),
														aux_join(Op, T1, T2, R).


% Predicat folosit in cadrul query-ului "join".
join_op(Op, NewCols, T1, T2, [NewCols|R]) :- aux_join(Op, T1, T2, R).


/**
  * Predicat folosit in cadrul query-ului "tfilter". Pentru a nu lega lista de
  * variabile anonime la randul curent din tabel, vom incerca satisfacerea
  * predicatului primit de catre "filter_op" in interiorul unui "not". Daca
  * "not" se va evalua la "true", stim sigur ca predicatul s-a evaluat la
  * "false", prin urmare nu includem acest rand din tabel in tabelul rezultat.
  * Daca "not" nu este satisfacut, predicatul a fost satisfacut si vom include
  * randul curent din tabel in rezultat.
  */
filter_op([], _, _, []).
filter_op([H|T], Vars, Pred, R) :- not((Vars = H, call(Pred))),
												filter_op(T, Vars, Pred, R), !.
filter_op([H|T], Vars, Pred, [H|R]) :- filter_op(T, Vars, Pred, R), !.


/**
  * Predicatul eval pentru fiecare tip de query. Pentru query-urile care primesc
  * la randul lor un query, se va satisface intai eval pe acest query, care va
  * returna un tabel asupra caruia vor fi aplicate operatiile corespunzatoare
  * fiecarui tip de query, cu ajutorul predicatelor auxiliare cerute. Voi
  * detalia doar complex_query2, unde consider ca sunt necesare anumite
  * explicatii suplimentare.
  *
  * Pentru complex_query2, am introdus un aux_query, care primeste doua
  * query-uri, anume table(movies) si tabelul format din coloanele movie_id
  * si rating, extrase din tabelul ratings, cu un select query. Acest query
  * va folosi predicate auxiliare definite si explicate mai sus, pentru a
  * intoarce tabelul format prin concatenarea la tabelul movies a coloanei de
  * rating, cu rating-ul aferent fiecarui film. Acest tabel va fi trecut
  * printr-un tfilter, care va intoarce tabelul cu filmele care respecta
  * restrictiile precizate in cerinta.
  */
eval(table(L), R) :- table_name(L, R).
eval(tprint(Q), Res) :- eval(Q, Res), print_table_op(Res).
eval(select(Columns, Q), Resp) :- eval(Q, R), trans(R, Rp),
							select_op(Rp, Columns, Res), trans(Res, Resp).
eval(join(Pred, Cols, Q1, Q2), Res) :- eval(Q1, [_|R1]), eval(Q2, [_|R2]),
											join_op(Pred, Cols, R1, R2, Res).
eval(tfilter(S, G, Q), [H|Resp]) :-
								eval(Q, [H|Res]), filter_op(Res, S, G, Resp).
eval(complex_query1(Q), R) :- 
	eval(tfilter([_, Name|_], string_concat(_, "escu", Name), tfilter([_, _|Rp],
		(length(Rp, L), sum(Rp, S), S / L > 5),
			tfilter([_, _, AA, PP|_], ((AA + PP) / 2 > 6), Q))), R). 
eval(complex_query2(G, L, U), R) :- eval(tfilter([_, _, Gen, Rtg],
	((string_concat(K, _, Gen), string_concat(_, G, K), !), Rtg >= L, Rtg =< U),
		aux_query(table(movies),
			select(["movie_id", "rating"], table(ratings)))), R), !.
eval(aux_query(Q1, Q2), R) :-
	eval(Q1, [H1|R1]), eval(Q2, [H2|R2]),
		complex_q2(R1, R2, Rp), append_q2([H1|R1], [H2|Rp], R), !.
