:- module(rdf_axioms, []).

/** <module> RDF axioms

This module asserts the RDF axioms as defined in Section 8 of the RDF
1.1 Semantics specification.

@auhtor Wouter Beek
@see https://www.w3.org/TR/rdf11-mt/
@tbd How to support infinite container membership property axioms?
@version 2018
*/

:- multifile
    rdf:axiom/4,
    rdf:container_membership_property/1.

:- initialization
   forall(
     rdf:container_membership_property(P),
     assert(rdf:axiom(rdf, P, rdf:type, rdf:'Property'))
   ).

/*
rdf:axiom(rdf, rdf:type,      rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:subject,   rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:predicate, rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:object,    rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:first,     rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:rest,      rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:value,     rdf:type, rdf:'Property').
rdf:axiom(rdf, rdf:nil,       rdf:type, rdf:'List').
*/

rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#object', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#value', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#List').
