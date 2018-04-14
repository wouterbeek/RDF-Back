:- module(
  rdf_ent_pp,
  [
    rdf_pp_argument//3, % +Premises, +Rule, +Conclusion
    rdf_pp_argument//4  % +Premises, +Rule, +Conclusion, +Options
  ]
).

/** <module> RDF entailment pretty-print

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(dcg)).
:- use_module(library(sw/rdf_print)).




%! rdf_pp_argument(+Premises:list(rdf_gen_triple), +Rule:compound,
%!                 +Conclusion:rdf_gen_triple)// is det.
%! rdf_pp_argument(+Premises:list(rdf_gen_triple), +Rule:compound,
%!                 +Conclusion:rdf_gen_triple, +Options:dict)// is det.
%
% [P] rdfs:subClassOf     rdfs:range      [C] rdfs:Class
% [_] rdfs:Datatype   [P] rdfs:subClassOf [I] rdfs:Class
% ------------------------------------------------------
% [I] rdfs:Class          rdf:type        [C] rdfs:Class
%
% The following options are defined:
%
%   * indent(nonneg) Default is 0.

rdf_pp_argument(Premises, Rule, Conclusion) -->
  rdf_pp_argument(Premises, Rule, Conclusion, _{indent: 0}).


rdf_pp_argument(Premises, Rule, Conclusion, Options) -->
  indent_(Options),
  rule(Rule),
  conclusion(Conclusion),
  premises(Premises, Options).

conclusion(TP) -->
  rdf_dcg_triple_pattern(TP).

indent_(Options) -->
  {get_dict(indent, Options, N)},
  indent(N).

premises([H|T], Options) --> !,
  premise(H, Options),
  premises(T, Options).
premises([], _) --> "".

premise(TP, Options) -->
  indent_(Options),
  rdf_dcg_triple_pattern(TP),
  nl.

rule(Rule) -->
  {rule_label(Rule, Label)},
  "[",
  atom(Label),
  "] ".

rule_label(Rule, Label) :-
  format(string(Label), "~w", [Rule]).
