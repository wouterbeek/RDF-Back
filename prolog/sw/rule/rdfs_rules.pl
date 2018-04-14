:- module(rdfs_rules, []).

/** <module> RDF entailment rules

This module asserts the RDF entailment rules (or ‘patters’) from
Section 9.2.1 of the RDF 1.1 Semantics specification.

@author Wouter Beek
@see https://www.w3.org/TR/rdf11-mt
@tbd The exclusion relation between datatype IRIs.
@version 2018
*/

:- use_module(library(sw/rule/rdf_rules)).

:- multifile
    rdf:recognized_datatype_iri/1,
    rdf:rule/3.

/*
rdf:rule(rdfs(1),    rdf(D,rdf:type,rdfs:'Datatype'),        []) :-
  rdf:recognized_datatype_iri(D).  
rdf:rule(rdfs(2),    rdf(I,rdf:type,C),                      [rdf(P,rdfs:domain,C),rdf(I,P,_)]).
rdf:rule(rdfs(3),    rdf(I,rdf:type,C),                      [rdf(P,rdfs:range,C),rdf(_,P,I)]).
rdf:rule(rdfs('4a'), rdf(I,rdf:type,rdfs:'Resource'),        [rdf(I,_,_)]).
rdf:rule(rdfs('4b'), rdf(I,rdf:type,rdfs:'Resource'),        [rdf(_,_,I)]).
rdf:rule(rdfs(5),    rdf(P, rdfs:subPropertyOf, R),          [rdf(P,rdfs:subPropertyOf,Q),rdf(Q,rdfs:subPropertyOf,R)]).
rdf:rule(rdfs(6),    rdf(P, rdfs:subPropertyOf, P),          [rdf(P,rdf:type,rdf:'Property')]).
rdf:rule(rdfs(7),    rdf(X,Q,Y),                             [rdf(P,rdfs:subPropertyOf,Q),rdf(X,P,Y)]).
rdf:rule(rdfs(8),    rdf(C,rdfs:subClassOf,rdfs:'Resource'), [rdf(C,rdf:type,rdfs:'Class')]).
rdf:rule(rdfs(9),    rdf(I,rdf:type,D),                      [rdf(C,rdfs:subClassOf,D),rdf(I,rdf:type,C)]).
rdf:rule(rdfs(10),   rdf(C,rdfs:subClassOf,C),               [rdf(C,rdf:type,rdfs:'Class')]).
rdf:rule(rdfs(11),   rdf(C,rdfs:subClassOf,E),               [rdf(C,rdfs:subClassOf,D),rdf(D,rdfs:subClassOf,E)]).
rdf:rule(rdfs(12),   rdf(P,rdfs:subPropertyOf,rdfs:member),  [rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')]).
rdf:rule(rdfs(13),   rdf(C,rdfs:subClassOf,rdfs:'Literal'),  [rdf(C,rdf:type,rdfs:'Datatype')]).
*/

%rdf:rule(rdfs(1), rdf(D,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Datatype'), []) :-
%  rdf:recognized_datatype_iri(D).
%rdf:rule(rdfs(2), rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C), [rdf(P,'http://www.w3.org/2000/01/rdf-schema#domain',C),rdf(I,P,_)]).
rdf:rule(rdfs(3), rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C), [rdf(P,'http://www.w3.org/2000/01/rdf-schema#range',C),rdf(_,P,I)]).
%rdf:rule(rdfs('4a'), rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Resource'), [rdf(I,_,_)]).
%rdf:rule(rdfs('4b'), rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Resource'), [rdf(_,_,I)]).
rdf:rule(rdfs(5), rdf(P, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', R), [rdf(P,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',Q),rdf(Q,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',R)]).
rdf:rule(rdfs(6), rdf(P, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P), [rdf(P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property')]).
rdf:rule(rdfs(7), rdf(X,Q,Y), [rdf(P,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',Q),rdf(X,P,Y)]).
%rdf:rule(rdfs(8), rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/2000/01/rdf-schema#Resource'), [rdf(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Class')]).
%rdf:rule(rdfs(9), rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D), [rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf',D),rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C)]).
%rdf:rule(rdfs(10), rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C), [rdf(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Class')]).
%rdf:rule(rdfs(11), rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf',E), [rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf',D),rdf(D,'http://www.w3.org/2000/01/rdf-schema#subClassOf',E)]).
%rdf:rule(rdfs(12), rdf(P,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf','http://www.w3.org/2000/01/rdf-schema#member'),  [rdf(P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty')]).
%rdf:rule(rdfs(13), rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/2000/01/rdf-schema#Literal'),  [rdf(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Datatype')]).
