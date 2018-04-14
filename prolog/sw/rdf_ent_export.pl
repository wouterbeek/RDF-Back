:- module(
  rdf_ent_export,
  [
    rdf_export_tree/1, % +Tree
    rdf_export_tree/2  % +Out, +Tree
  ]
).

/** <module> RDF entailment: export

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(graph/gv)).
:- use_module(library(sw/rdf_ent_pp)).
:- use_module(library(sw/rdf_print)).





%! rdf_export_tree(+Tree:compound) is det.
%! rdf_export_tree(+Out:stream, +Tree:compound) is det.

rdf_export_tree(Tree) :-
  gv_show({Tree}/[Out]>>rdf_export_tree(Out, Tree)).


rdf_export_tree(Out, Tree) :-
  format(Out, "digraph g {\n", []),
  rdf_export_tree_(Out, Tree),
  format(Out, "}\n", []).

rdf_export_tree_(Out, t(Conclusion,Rule,Premises)) :-
  gv_id(Conclusion, X),
  dcg_with_output_to(
    string(ConclusionLabel),
    rdf_dcg_generalized_triple_pattern(Conclusion)
  ),
  gv_node(Out, X, [label(ConclusionLabel)]),
  gv_id(Conclusion-Rule, Y),
  rule_label(Rule, RuleLabel),
  gv_node(Out, Y, [label(RuleLabel)]),
  gv_edge(Out, X, Y),
  maplist(rdf_export_subtree_(Out, Y), Premises).

rdf_export_subtree_(Out, Y, t(Conclusion,Rule,Premises)) :-
  gv_id(Conclusion, Z),
  dcg_with_output_to(
    string(ConclusionLabel),
    rdf_dcg_generalized_triple_pattern(Conclusion)
  ),
  gv_node(Out, Z, [label(ConclusionLabel)]),
  gv_edge(Out, Y, Z),
  rdf_export_tree_(Out, t(Conclusion,Rule,Premises)).
