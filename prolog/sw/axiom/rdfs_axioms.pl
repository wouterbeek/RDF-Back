:- module(rdfs_axioms, []).

/** <module> RDFS axioms

This module asserts the RDF axioms as defined in Section 9 of the RDF
1.1 Semantics specification.

@auhtor Wouter Beek
@see https://www.w3.org/TR/rdf11-mt/
@tbd How to support infinite container membership property axioms?
@version 2018
*/

:- use_module(library(sw/axiom/rdf_axioms)).

:- multifile
    rdf:axiom/4,
    rdf:container_membership_property/1.

:- initialization
   forall(
     rdf:container_membership_property(P),
     (
       assert(rdf:axiom(rdfs, P, rdf:type, rdfs:'ContainerMembershipProperty')),
       assert(rdf:axiom(rdfs, P, rdfs:domain, rdfs:'Resource')),
       assert(rdf:axiom(rdfs, P, rdfs:range, rdfs:'Resource'))
     )
   ).

/*
rdf:axiom(rdfs, rdf:type,           rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdfs:domain,        rdfs:domain,        rdf:'Property').
rdf:axiom(rdfs, rdfs:range,         rdfs:domain,        rdf:'Property').
rdf:axiom(rdfs, rdfs:subPropertyOf, rdfs:domain,        rdf:'Property').
rdf:axiom(rdfs, rdfs:subClassOf,    rdfs:domain,        rdfs:'Class').
rdf:axiom(rdfs, rdf:subject,        rdfs:domain,        rdf:'Statement').
rdf:axiom(rdfs, rdf:predicate,      rdfs:domain,        rdf:'Statement').
rdf:axiom(rdfs, rdf:object,         rdfs:domain,        rdf:'Statement').
rdf:axiom(rdfs, rdfs:member,        rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdf:first,          rdfs:domain,        rdf:'List').
rdf:axiom(rdfs, rdf:rest,           rdfs:domain,        rdf:'List').
rdf:axiom(rdfs, rdfs:seeAlso,       rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdfs:isDefinedBy,   rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdfs:comment,       rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdfs:label,         rdfs:domain,        rdfs:'Resource').
rdf:axiom(rdfs, rdf:value,          rdfs:domain,        rdfs:'Resource').

rdf:axiom(rdfs, rdf:type,           rdfs:range,         rdfs:'Class').
rdf:axiom(rdfs, rdfs:domain,        rdfs:range,         rdfs:'Class').
rdf:axiom(rdfs, rdfs:range,         rdfs:range,         rdfs:'Class').
rdf:axiom(rdfs, rdfs:subPropertyOf, rdfs:range,         rdf:'Property').
rdf:axiom(rdfs, rdfs:subClassOf,    rdfs:range,         rdfs:'Class').
rdf:axiom(rdfs, rdf:subject,        rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdf:predicate,      rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdf:object,         rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdfs:member,        rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdf:first,          rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdf:rest,           rdfs:range,         rdf:'List').
rdf:axiom(rdfs, rdfs:seeAlso,       rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdfs:isDefinedBy,   rdfs:range,         rdfs:'Resource').
rdf:axiom(rdfs, rdfs:comment,       rdfs:range,         rdfs:'Literal').
rdf:axiom(rdfs, rdfs:label,         rdfs:range,         rdfs:'Literal').
rdf:axiom(rdfs, rdf:value,          rdfs:range,         rdfs:'Resource').

rdf:axiom(rdfs, rdf:'Alt',          rdfs:subClassOf,    rdfs:'Container').
rdf:axiom(rdfs, rdf:'Bag',          rdfs:subClassOf,    rdfs:'Container').
rdf:axiom(rdfs, rdf:'Seq',          rdfs:subClassOf,    rdfs:'Container').
rdf:axiom(rdfs, rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').

rdf:axiom(rdfs, rdfs:isDefinedBy,   rdfs:subPropertyOf, rdfs:seeAlso).

rdf:axiom(rdfs, rdfs:'Datatype',    rdfs:subClassOf,    rdfs:'Class').
*/

rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Class').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#object', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#member', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#List').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#List').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#isDefinedBy', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#value', 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#Resource').

rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Class').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#domain', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Class').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Class').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Class').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#object', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#member', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#List').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#isDefinedBy', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Literal').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Literal').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#value', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://www.w3.org/2000/01/rdf-schema#Resource').

rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt', 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/2000/01/rdf-schema#Container').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag', 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/2000/01/rdf-schema#Container').
rdf:axiom(rdfs, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq', 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/2000/01/rdf-schema#Container').
rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty', 'http://www.w3.org/2000/01/rdf-schema#subClassOf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').

rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#isDefinedBy', 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', 'http://www.w3.org/2000/01/rdf-schema#seeAlso').

rdf:axiom(rdfs, 'http://www.w3.org/2000/01/rdf-schema#Datatype', 'http://www.w3.org/2000/01/rdf-schema#subClassOf',    'http://www.w3.org/2000/01/rdf-schema#Class').
