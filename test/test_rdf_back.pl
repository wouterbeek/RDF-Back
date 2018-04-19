:- module(
  test_rdf_back,
  [
    test/0,test2/0
  ]
).

/** <module> Testing RDF backward chaining

@author Wouter Beek
@version 2018
*/

:- use_module(library(sw/rdf_back)).
:- use_module(library(sw/rdf_ent_export)).

% configuration
:- use_module(library(sw/axiom/rdfs_axioms)).
:- use_module(library(sw/rule/rdfs_rules)).





test :-
  rdf_back(
    rdf(rdfs:'Class',rdf:type,rdfs:'Class'),
    _,
    _{bindings: true, debug: false, sort_premises: false}
  ).

test2 :-
  rdf_back(
    rdf(rdfs:'Class',rdf:type,rdfs:'Class'),
    Tree,
    _{bindings: true, debug: true, sort_premises: false}
  ),
  rdf_export_tree(Tree).
