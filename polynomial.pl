pvars([x,y,z]).

pvar(X):-pvars(V),member(X,V).

power(X):-pvar(X),!.

power(X^Y):-pvar(X),integer(Y),Y>1,!.
power(X^Y):-number(X),integer(Y),Y>1,!.

coefficient(K):-number(K).



%monomial

monomial(X):-number(X),!.
monomial(X):-pvar(X),!.
monomial(-X):-power(X),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.

%helper deleting unwanted coefficients

delete_coefficient(0*_,0):-!.
delete_coefficient(K*X,-X):-monomial(K*X),J is K+1,J==0,!.
delete_coefficient(K*X,X):-monomial(K*X),K==1,!.
delete_coefficient(K*X,K*X):-monomial(K*X),!.
delete_coefficient(X,X):-monomial(X),!.


%helper adding wanted coefficients

add_coefficient(-X,-1*X):-!.
add_coefficient(-1*X,-1*X):-!.
add_coefficient(1*X,1*X):-!.
add_coefficient(K*X,K*X):-!.
add_coefficient(X,1*X):-!.

%not used, able to change K*X X*K if number(K)
 
change_coefficient(K*X,X*K):-number(X),pvar(K),!.
change_coefficient(K*X,K*X):-!.

%polynomial

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.
polynomial(P-M):-monomial(M),polynomial(P),!.

%operations on polynomials

%adding monomials.


monomial_add(0*_,K*Y,K*Y):-!. 
monomial_add(K*Y,0*Y,K*Y):-!. 
monomial_add(K*X,-Y*X,0):-K+Y==0.
monomial_add(K*X,S*X,T*X):-T is K+S,!.
monomial_add(X,S*X,T*X):-T is S+1,!.
monomial_add(K*X,X,T*X):-T is K+1,!.
monomial_add(X,Y,J):-number(X),number(Y),J is X+Y,!.
monomial_add(X,Y,R):-add_coefficient(X,S*H),add_coefficient(Y,A*U),H==U,T is S+A,delete_coefficient(T*U,R),!.
%monomial_add(X,X,2*X):-!.



%multiplying a monomial by a scalar

monomial_mult(0,_,0):-!.
monomial_mult(X,Y,J):-number(X),number(Y),J is X*Y,!.
monomial_mult(X,K*Y,J*Y):-number(X),J is K*X,!.
monomial_mult(X,-Y,H*Y):-number(X),X\=1,H is -1*X,!.
monomial_mult(X,Y,X*Y):-number(X),X\=1,!.
%monomial_mult(X,Y,Z):-add_coefficient(Y,A),monomial_mult(X,A),!.
monomial_mult(X,Y,Y):-number(X),X==1,!.


%adding a monomial to a polynomial


monom_add_polynom([X],[Y|Ys],[M|Ys]):-monomial_add(X,Y,S),delete_coefficient(S,M),monom_add_polynom([],_,_),!.
monom_add_polynom([X],[Y|Ys],[Y|Z]):-monom_add_polynom([X],Ys,Z),!. 
monom_add_polynom([0],[Y|Ys],[Y|Ys]):-!.
monom_add_polynom([X],[0],[X]):-!.
monom_add_polynom([],_,_). 

%sign of a number

negativeMonomial(X):-number(X),X<0,!.
negativeMonomial(K*_):-K<0,!.
negativeMonomial(X):-add_coefficient(X,K*_),K<0,!.



%%%%%% 1 %%%%%%%

poly2list(X,[X]):-monomial(X),!.

%%listas para expressões

poly2list(P-K*S,[W*S|Ys]):-monomial(W*S),negativeMonomial(W*S),K is -1*W,poly2list(P,Ys),!.
poly2list(P-H,[M|Ys]):-monomial(M),number(M),negativeMonomial(M),H is -1*M,poly2list(P,Ys),!.  % poly2list(T,[-5,3,-x]). T = -x- -3-5. negativeMonomial(M)!!!
poly2list(P-H*O,[M|Ys]):-monomial(M),negativeMonomial(M),add_coefficient(M,Y*O),H is -1*Y,poly2list(P,Ys),!.

poly2list(P+M,[M|Ys]):-monomial(M),poly2list(P,Ys),!.

%expressões para listas

poly2list(P-K*S,[H*S|Ys]):-monomial(K*S),H is -1*K,poly2list(P,Ys),!.
poly2list(P-M,[H|Ys]):-monomial(M),number(M),H is -1*M,poly2list(P,Ys),!.
poly2list(P-M,[-M|Ys]):-monomial(M),poly2list(P,Ys),!.


%%%%%%%%%%%%%%%%

%%%%% 2 %%%%%%%%


simpoly_list(L,R):-acc(L,[],R),!.

acc([H|T],S,Y):-monom_add_polynom([H],T,Z),acc(Z,S,Y),!.
acc([H|T],S,Y):-acc(T,[H|S],Y),!.
acc([],Z,Z):-!.

%%%%%%%%%%%%%%%%%

%%%%%%% 3 %%%%%%%


simpoly(X,Y):-poly2list(X,Z),simpoly_list(Z,A),poly2list(Y,A).


%%%%%%%%%%%%%%%%%

%%%%% 4 %%%%%%%%%

scalepoly(E,S,Z):-poly2list(E,R),scale(R,S,Y),simpoly_list(Y,H),poly2list(Z,H).


scale([M],S,[Y]):-monomial_mult(S,M,Y),!.
scale(_,0,[0]):-!.
scale([M|L1],S,[R|L2]):- monomial_mult(S,M,R), scale(L1,S,L2).


%%%%%%%%%%%%%%%%%

%%%%%%% 5 %%%%%%%


addpoly(E1,E2,E3):-poly2list(E1,L1),poly2list(E2,L2),append(L1,L2,L3),simpoly_list(L3,L4),poly2list(E3,L4).

append([],L,L).
append([H|T],L2,[H|L3]):-append(T,L2,L3). 


%%%%%%%%%%%%%%%%%