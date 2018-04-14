:- module(
  rdf_back,
  [
    rdf_back/1,        % ?Triple
    rdf_back/2,        % ?Triple, -Tree
    rdf_back/3,        % ?Triple, -Tree, +Options
    rdf_back_triple/3, % ?S, ?P, ?O
    rdf_back_triple/4  % ?S, ?P, ?O, -Tree
  ]
).

/** <module> RDF(S) entailment through backward chaining

# Configuration

You must load the axiom modules you want to use _after_ you have
loaded your backends.  This is specifically needed for the RDF and
RDFS axiom, which are more/less plentyfull based on the data over
which entailment is performed.

# Generalized RDF

Notice that not all RDF(S) entailments can be expressed in RDF; some
require generalized RDF.

# Debugging

Enable debug messages with `debug(rdf_back)'.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
%:- use_module(library(tabling)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(sw/rdf_ent_pp)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_print)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(term_ext)).

:- rdf_meta
   rdf:axiom(?, r, r, o),
   rdf_back(t),
   rdf_back(t, -),
   rdf_back(t, -, +),
   rdf_back_triple(r, r, o),
   rdf_back_triple(r, r, o, -),
   rdf:rule(?, t, t).

%:- table rdf_back_/4.

rdf:rule(axiom(Module), rdf(S, P, O), []) :-
  rdf:axiom(Module, S, P, O).





%! rdf_back(?Triple:rdf_triple) is nondet.
%! rdf_back(?Triple:rdf_triple, -Tree:compound) is nondet.
%! rdf_back(?Triple:rdf_triple, -Tree:compound, +Options:dict) is nondet.
%
% The following options are supported:
%
%   * bindings(+boolean)
%
%     Whether or not variable bindings are returned as part of the
%     proof tree.  Default is `false'.
%
%   * debug(+boolean)
%
%     Whether or not debug messages are printed.  Default is `false'.
%     This option is present as a temporary workaround for unexpected
%     debugging/1 behavior.
%
%   * sort_premises(+boolean)
%
%     Whether or not the premises are sorted.  Default is `false'.
%
% @tbd Why is debugging/1 showing up in profiling?  If debugging/1
%      would be compiled out, we could do away with option `debug'.

rdf_back(Conclusion) :-
  rdf_back(Conclusion, _).


rdf_back(Conclusion, Tree) :-
  rdf_back(Conclusion, Tree, _{}).


rdf_back(Conclusion, Tree, Options) :-
  rdf_back_(Options, [], Conclusion, Tree).


rdf_back_(Options, Hist1, Conclusion, t(TP,Rule,Bindings,SubTrees)) :-
  (get_dict(bindings, Options, true) -> copy_term(Conclusion, TP) ; true),
  update_history(Hist1, Conclusion, Hist2),
  rdf:rule(Rule, Conclusion, Premises0),
  (   get_dict(sort_premises, Options, true)
  ->  groundsort(Premises0, Premises)
  ;   Premises = Premises0
  ),
  (   get_dict(bindings, Options, true)
  ->  unifiable(TP, Conclusion, Bindings)
  ;   Bindings = []
  ),
  (   get_dict(debug, Options, true)
  ->  length(Hist1, N),
      debug_phrase(
        rdf_back,
        rdf_pp_argument(Premises, Rule, Bindings, TP, _{indent: N})
      )
  ;   true
  ),
  maplist(rdf_back_(Options, Hist2), Premises, SubTrees).

update_history(Hist, Generic, [Generic|Hist]) :-
  (   member(Specific, Hist),
      subsumes_term(Generic, Specific)
  ->  debug(rdf_back, "❌", []),
      fail
  ;   true
  ).



%! rdf_back_triple(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.
%! rdf_back_triple(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -Tree:compound) is nondet.

rdf_back_triple(S, P, O) :-
  rdf_back_triple(S, P, O, _).


rdf_back_triple(S, P, O, Tree) :-
  rdf_back(rdf(S,P,O), Tree).
