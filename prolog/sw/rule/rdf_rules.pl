:- module(rdf_rules, []).

/** <module> RDF entailment rules

This module asserts the RDF entailment rules (or ‘patters’) from
Section 8.1.1 of the RDF 1.1 Semantics specification.

@author Wouter Beek
@see https://www.w3.org/TR/rdf11-mt
@version 2018
*/

:- multifile
    rdf:recognized_datatype_iri/1,
    rdf:rule/3.

/*
rdf:recognized_datatype_iri(xsd:string).

rdf:rule(rdf(1), rdf(literal(lang(LTag,Lex)),rdf:type,rdf:langString), [rdf(_S,_P,literal(lang(LTag,Lex)))]).
rdf:rule(rdf(1), rdf(literal(type(D,Lex)),rdf:type,D), [rdf(_S,_P,literal(type(D,Lex)))]) :-
  rdf:recognized_datatype_iri(D).
rdf:rule(rdf(2), rdf(P,rdf:type,rdf:'Property'), [rdf(_S,P,_O)]).
*/

rdf:recognized_datatype_iri('http://www.w3.org/2001/XMLSchema#string').

rdf:rule(rdf(1), rdf(literal(lang(LTag,Lex)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'), [rdf(_S,_P,literal(lang(LTag,Lex)))]).
rdf:rule(rdf(1), rdf(literal(type(D,Lex)),'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',D), [rdf(_S,_P,literal(type(D,Lex)))]) :-
  rdf:recognized_datatype_iri(D).
rdf:rule(rdf(2), rdf(P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'), [rdf(_S,P,_O)]).
