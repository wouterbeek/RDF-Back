:- module(
  test_rdf_back,
  [
    test/0
  ]
).

/** <module> Testing RDF backward chaining

@author Wouter Beek
@version 2018
*/

:- use_module(library(sw/rdf_back)).

% configuration
:- use_module(library(sw/axiom/rdfs_axioms)).
:- use_module(library(sw/rule/rdfs_rules)).

:- dynamic
    axiom/4,
    recognized_datatype_iri/1,
    rule/3.

:- multifile
    axiom/4,
    recognized_datatype_iri/1,
    rule/3.

test :-
  rdf_back(rdf(rdfs:'Class',rdf:type,rdfs:'Class')).
