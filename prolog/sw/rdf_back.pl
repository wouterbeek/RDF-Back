:- module(
  rdf_back,
  [
    rdf_back/1, % ?Triple
    rdf_back/2, % ?Triple, -Tree
    rdf_back/3, % ?S, ?P, ?O
    rdf_back/4  % ?S, ?P, ?O, -Tree
  ]
).

/** <module> RDF(S) entailment through backward chaining

You must load the axiom modules you want to use _after_ you have
loaded your backends.  This is specifically needed for the RDF and
RDFS axiom, which are more/less plentyfull based on the data over
which entailment is performed.

Notice that not all RDF(S) entailments can be expressed in RDF; some
require generalized RDF.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(debug)).

:- use_module(library(dcg)).
:- use_module(library(sw/rdf_ent_pp)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- debug(rdf_back).

:- rdf_meta
   rdf:axiom(?, r, r, o),
   rdf_back(t),
   rdf_back(t, -),
   rdf_back(r, r, o),
   rdf_back(r, r, o, -),
   rdf:rule(?, t, t).

rdf:rule(axiom(Module), rdf(S, P, O), []) :-
  rdf:axiom(Module, S, P, O).





%! rdf_back(?Triple:rdf_triple) is nondet.
%! rdf_back(?Triple:rdf_triple, -Tree:compound) is nondet.
%! rdf_back(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.
%! rdf_back(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -Tree:compound) is nondet.

rdf_back(Conclusion) :-
  rdf_back(Conclusion, _).


rdf_back(Conclusion, Tree) :-
  rdf_back_([], Conclusion, Tree).


rdf_back(S, P, O) :-
  rdf_back(rdf(S,P,O), _).


rdf_back(S, P, O, Tree) :-
  rdf_back(rdf(S,P,O), Tree).

rdf_back_(Hist1, Conclusion, t(Conclusion,Rule,SubTrees)) :-
  copy_term(Conclusion, Item),
  rdf_back_history_(Hist1, Item, Hist2),
  rdf:rule(Rule, Conclusion, Premises),
  (   debugging(rdf_back)
  ->  length(Hist1, N),
      dcg_with_output_to(
        user_output,
        rdf_pp_argument(Premises, Rule, Conclusion, _{indent: N})
      )
  ;   true
  ),
  maplist(rdf_back_(Hist2), Premises, SubTrees).

% If `X' is an instance of a Triple Fragment in `L1', then replace
% that Triple Fragment with `X'.
rdf_back_history_(T, X, L) :-
  X =.. [rdf|L1],
  rdf_back_history_list_(L1, [x,y,z], L2),
  Y =.. [rdf|L2],
  (memberchk(Y, T) -> fail ; L = [Y|T]).

rdf_back_history_list_([H1|T1], [H2|T2], [H1|T3]) :-
  var(H1), !,
  H1 = H2,
  rdf_back_history_list_(T1, T2, T3).
rdf_back_history_list_([H1|T1], L2, [H1|T3]) :- !,
  rdf_back_history_list_(T1, L2, T3).
rdf_back_history_list_([], _, []).
