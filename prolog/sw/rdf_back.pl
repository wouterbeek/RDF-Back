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

# Configuration

You must load the axiom modules you want to use _after_ you have
loaded your backends.  This is specifically needed for the RDF and
RDFS axiom, which are more/less plentyfull based on the data over
which entailment is performed.

# Generalized RDF

Notice that not all RDF(S) entailments can be expressed in RDF; some
require generalized RDF.

# Debugging

Enable debug messages with `debug(rdf_back)'.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(sw/rdf_ent_pp)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_print)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(term_ext)).

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

rdf_back_(Hist1, Conclusion, t(TP,Rule,Bindings,SubTrees)) :-
  copy_term(Conclusion, TP),
  update_history(Hist1, TP, Hist2),
  rdf:rule(Rule, Conclusion, Premises),
  groundsort(Premises, SortedPremises),
  unifiable(TP, Conclusion, Bindings),
  (   debugging(rdf_back)
  ->  length(Hist1, N),
      debug_phrase(
        rdf_back,
        rdf_pp_argument(SortedPremises, Rule, Bindings, TP, _{indent: N})
      )
  ;   true
  ),
  maplist(rdf_back_(Hist2), SortedPremises, SubTrees).

update_history(Hist, Generic, [Generic|Hist]) :-
  (   member(Specific, Hist),
      subsumes_term(Generic, Specific)
  ->  debug(rdf_back, "❌", []),
      fail
  ;   true
  ).
