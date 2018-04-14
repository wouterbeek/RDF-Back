:- module(
  rdf_ent_pp,
  [
    rdf_pp_argument//4, % +Premises, +Rule, +Bindings, +Conclusion
    rdf_pp_argument//5, % +Premises, +Rule, +Bindings, +Conclusion, +Options
    rdf_pp_rule//2      % +Rule, +Bindings
  ]
).

/** <module> RDF entailment pretty-print

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(dcg)).
:- use_module(library(sw/rdf_print)).





%! rdf_pp_argument(+Premises:list(rdf_gen_triple), +Rule:compound, +Bindings:list,
%!                 +Conclusion:rdf_gen_triple)// is det.
%! rdf_pp_argument(+Premises:list(rdf_gen_triple), +Rule:compound, +Bindings:list,
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

rdf_pp_argument(Premises, Rule, Bindings, Conclusion) -->
  rdf_pp_argument(Premises, Rule, Bindings, Conclusion, _{indent: 0}).


rdf_pp_argument(Premises, Rule, Bindings, Conclusion, Options) -->
  indent_(Options),
  rdf_pp_rule(Rule, Bindings),
  " ",
  conclusion(Conclusion),
  nl,
  'premise*'(Premises, Options).

conclusion(TP) -->
  rdf_dcg_generalized_triple_pattern(TP).

indent_(Options) -->
  {get_dict(indent, Options, N)},
  indent(N).

'premise*'([H|T], Options) --> !,
  indent_(Options),
  premise(H),
  nl,
  'premise*'(T, Options).
'premise*'([], _) --> "".

premise(TP) -->
  rdf_dcg_generalized_triple_pattern(TP).



%! rdf_pp_rule(+Rule:compound, +Bindings:list(compound))// is det.

rdf_pp_rule(Rule, Bindings) -->
  "[",
  term(Rule),
  ({Bindings == []} -> "" ; ":", 'binding+'(Bindings)),
  "]".

binding(X=Y) -->
  term(X),
  "=",
  rdf_dcg_term_or_var(Y).

'binding+'([H|T]) --> !,
  binding(H),
  'binding*'(T).

'binding*'([H|T]) --> !,
  ",",
  binding(H),
  'binding*'(T).
'binding*'([]) --> "".
