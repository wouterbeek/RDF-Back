:- module(hdt_backend, []).

/** <module> HDT backend

@author Wouter Beek
@version 2018
*/

:- use_module(library(sw/hdt_db)).
:- use_module(library(sw/hdt_graph)).
:- use_module(library(sw/rdf_term)).

:- multifile
    rdf:container_membership_property/1,
    rdf:rule/3.

rdf:container_membership_property(P) :-
  hdt_default(Hdt),
  hdt_term_prefix(Hdt, predicate, rdf:'_', P),
  rdf_is_container_membership_property(P).

rdf:rule(backend(hdt), rdf(S, P, O), []) :-
  hdt_default(Hdt),
  hdt_triple(Hdt, S, P, O).
