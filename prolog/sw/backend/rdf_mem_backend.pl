:- module(rdf_mem_backend, []).

/** <module> RDF memory backend

@author Wouter Beek
@version 2018
*/

:- use_module(library(semweb/rdf11_containers), []).

:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_term)).

:- multifile
    rdf:container_membership_property/1,
    rdf:rule/3.

rdf:container_membership_property(P) :-
  rdf_predicate(P),
  rdf_is_container_membership_property(P).

rdf:rule(db(rdf_mem), rdf(S, P, O), []) :-
  rdf_triple(S, P, O).
