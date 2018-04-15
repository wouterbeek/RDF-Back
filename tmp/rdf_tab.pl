:- module(rdf_tab, [rdf_tab/1]).

:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(library(sw/hdt_db)).
:- use_module(library(sw/hdt_graph)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_term)).

:- dynamic
    axiom/3.

:- initialization
   forall(
     container_membership_property(P),
     assert(axiom(P, rdf:type, rdf:'Property'))
   ).

:- rdf_meta
   axiom(r, r, o),
   rdf_tab(t),
   recognized_datatype_iri(r),
   rule(t, t).

rdf_tab(C) :-
  rdf_tab([], C).

rdf_tab(T, H) :-
  (   memberchk(H, T)
  ->  fail
  ;   rule(H, Ps),
      maplist(rdf_tab([H|T]), Ps)
  ).

axiom(rdf:type, rdf:type, rdf:'Property').
axiom(rdf:subject, rdf:type, rdf:'Property').
axiom(rdf:predicate, rdf:type, rdf:'Property').
axiom(rdf:object, rdf:type, rdf:'Property').
axiom(rdf:first, rdf:type, rdf:'Property').
axiom(rdf:rest, rdf:type, rdf:'Property').
axiom(rdf:value, rdf:type, rdf:'Property').
axiom(rdf:nil, rdf:type, rdf:'List').
axiom(rdf:type, rdfs:domain, rdfs:'Resource').
axiom(rdfs:domain, rdfs:domain, rdf:'Property').
axiom(rdfs:range, rdfs:domain, rdf:'Property').
axiom(rdfs:subPropertyOf, rdfs:domain, rdf:'Property').
axiom(rdfs:subClassOf, rdfs:domain, rdfs:'Class').
axiom(rdf:subject, rdfs:domain, rdf:'Statement').
axiom(rdf:predicate, rdfs:domain, rdf:'Statement').
axiom(rdf:object, rdfs:domain, rdf:'Statement').
axiom(rdfs:member, rdfs:domain, rdfs:'Resource').
axiom(rdf:first, rdfs:domain, rdf:'List').
axiom(rdf:rest, rdfs:domain, rdf:'List').
axiom(rdfs:seeAlso, rdfs:domain, rdfs:'Resource').
axiom(rdfs:isDefinedBy, rdfs:domain, rdfs:'Resource').
axiom(rdfs:comment, rdfs:domain, rdfs:'Resource').
axiom(rdfs:label, rdfs:domain, rdfs:'Resource').
axiom(rdf:value, rdfs:domain, rdfs:'Resource').
axiom(rdf:type, rdfs:range, rdfs:'Class').
axiom(rdfs:domain, rdfs:range, rdfs:'Class').
axiom(rdfs:range, rdfs:range, rdfs:'Class').
axiom(rdfs:subPropertyOf, rdfs:range, rdf:'Property').
axiom(rdfs:subClassOf, rdfs:range, rdfs:'Class').
axiom(rdf:subject, rdfs:range, rdfs:'Resource').
axiom(rdf:predicate, rdfs:range, rdfs:'Resource').
axiom(rdf:object, rdfs:range, rdfs:'Resource').
axiom(rdfs:member, rdfs:range, rdfs:'Resource').
axiom(rdf:first, rdfs:range, rdfs:'Resource').
axiom(rdf:rest, rdfs:range, rdf:'List').
axiom(rdfs:seeAlso, rdfs:range, rdfs:'Resource').
axiom(rdfs:isDefinedBy, rdfs:range, rdfs:'Resource').
axiom(rdfs:comment, rdfs:range, rdfs:'Literal').
axiom(rdfs:label, rdfs:range, rdfs:'Literal').
axiom(rdf:value, rdfs:range, rdfs:'Resource').
axiom(rdf:'Alt', rdfs:subClassOf, rdfs:'Container').
axiom(rdf:'Bag', rdfs:subClassOf, rdfs:'Container').
axiom(rdf:'Seq', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').
axiom(rdfs:isDefinedBy, rdfs:subPropertyOf, rdfs:seeAlso).
axiom(rdfs:'Datatype', rdfs:subClassOf, rdfs:'Class').

container_membership_property(P) :-
  hdt_default(Hdt),
  hdt_term_prefix(Hdt, predicate, rdf:'_', P),
  rdf_is_container_membership_property(P).
container_membership_property(P) :-
  rdf_predicate(P),
  rdf_is_container_membership_property(P).

recognized_datatype_iri(xsd:string).

rule(rdf(S, P, O), []) :-
  axiom(S, P, O).
rule(rdf(literal(lang(LTag,Lex)),rdf:type,rdf:langString), [rdf(_S,_P,literal(lang(LTag,Lex)))]).
rule(rdf(literal(type(D,Lex)),rdf:type,D), [rdf(_S,_P,literal(type(D,Lex)))]) :-
  recognized_datatype_iri(D).
rule(rdf(P,rdf:type,rdf:'Property'), [rdf(_S,P,_O)]).
rule(rdf(D,rdf:type,rdfs:'Datatype'), []) :-
  recognized_datatype_iri(D).  
rule(rdf(I,rdf:type,C), [rdf(P,rdfs:domain,C),rdf(I,P,_)]).
rule(rdf(I,rdf:type,C), [rdf(P,rdfs:range,C),rdf(_,P,I)]).
rule(rdf(I,rdf:type,rdfs:'Resource'), [rdf(I,_,_)]).
rule(rdf(I,rdf:type,rdfs:'Resource'), [rdf(_,_,I)]).
rule(rdf(P, rdfs:subPropertyOf, R), [rdf(P,rdfs:subPropertyOf,Q),rdf(Q,rdfs:subPropertyOf,R)]).
rule(rdf(P, rdfs:subPropertyOf, P), [rdf(P,rdf:type,rdf:'Property')]).
rule(rdf(X,Q,Y), [rdf(P,rdfs:subPropertyOf,Q),rdf(X,P,Y)]).
rule(rdf(C,rdfs:subClassOf,rdfs:'Resource'), [rdf(C,rdf:type,rdfs:'Class')]).
rule(rdf(I,rdf:type,D), [rdf(C,rdfs:subClassOf,D),rdf(I,rdf:type,C)]).
rule(rdf(C,rdfs:subClassOf,C), [rdf(C,rdf:type,rdfs:'Class')]).
rule(rdf(C,rdfs:subClassOf,E), [rdf(C,rdfs:subClassOf,D),rdf(D,rdfs:subClassOf,E)]).
rule(rdf(P,rdfs:subPropertyOf,rdfs:member), [rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')]).
rule(rdf(C,rdfs:subClassOf,rdfs:'Literal'), [rdf(C,rdf:type,rdfs:'Datatype')]).
rule(rdf(S, P, O), []) :-
  hdt_default(Hdt),
  hdt_triple(Hdt, S, P, O).
rule(rdf(S, P, O), []) :-
  rdf_triple(S, P, O).
