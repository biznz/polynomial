pvars([x,y,z]).

pvar(X):-pvars(V),member(X,V).

power(X):-pvar(X),!.

power(X^Y):-pvar(X),integer(Y),Y>1,!.

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

%polynomial

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.
polynomial(P-M):-monomial(M),polynomial(P),!.

%operations on polynomials

%adding monomials.

monomial_add(0*_,X,Z):-delete_coefficient(X,Z),!. 
monomial_add(X,0*Y,Z):-delete_coefficient(X,Z),!.
monomial_add(X,0,Z):-delete_coefficient(X,Z),!.
monomial_add(0,X,Z):-delete_coefficient(X,Z),!.
monomial_add(X,Y,J):-number(X),number(Y),J is X+Y,!.
monomial_add(X,Y,R):-add_coefficient(X,S*H),add_coefficient(Y,A*U),H==U,T is S+A,delete_coefficient(T*U,R),!.

%multiplying a scalar by a monomial or scalar by scalar

monomial_mult(0,_,0):-!.
monomial_mult(_,0,0):-!.
monomial_mult(X,Y,J):-number(X),number(Y),J is X*Y,!.
monomial_mult(X,Y,Z):-number(X),add_coefficient(Y,S*H),J is X*S,delete_coefficient(J*H,Z),!.

%adding a monomial to a polynomial

monom_add_polynom(0,[Y|Ys],[Y|Ys]):-!.
monom_add_polynom(X,[],[X]):-!.
monom_add_polynom(X,[Y|Ys],[R|Ys]):-monomial_add(X,Y,R),!.
monom_add_polynom(X,[Y|Ys],[Y|Z]):-monom_add_polynom(X,Ys,Z),!.


%sign of a number

negativeMonomial(X):-number(X),X<0,!.
negativeMonomial(K*_):-K<0,!.
negativeMonomial(X):-add_coefficient(X,K*_),K<0,!.





%%%%%% 1 %%%%%%%

poly2list(X,[T]):-monomial(X),delete_coefficient(X,T),!.
poly2list(T,[X]):-monomial(X),delete_coefficient(X,T),!.

%list to expression

poly2list(P-R,[W*S|Ys]):-monomial(W*S),negativeMonomial(W*S),K is -1*W,delete_coefficient(K*S,R),poly2list(P,Ys),!.
poly2list(P-H,[M|Ys]):-monomial(M),number(M),negativeMonomial(M),H is -1*M,poly2list(P,Ys),!.  % poly2list(T,[-5,3,-x]). T = -x- -3-5. negativeMonomial(M)!!!
poly2list(P-F,[M|Ys]):-monomial(M),negativeMonomial(M),add_coefficient(M,Y*O),H is -1*Y,delete_coefficient(H*O,F),poly2list(P,Ys),!.

poly2list(P+M,[R|Ys]):-monomial(M),delete_coefficient(M,R),poly2list(P,Ys),!.
poly2list(P+R,[M|Ys]):-monomial(M),delete_coefficient(M,R),poly2list(P,Ys),!.

%expression to list

poly2list(P-K*S,[G|Ys]):-monomial(K*S),H is -1*K,delete_coefficient(H*S,G),poly2list(P,Ys),!.
poly2list(P-M,[H|Ys]):-monomial(M),number(M),H is -1*M,poly2list(P,Ys),!.
poly2list(P-M,[-M|Ys]):-monomial(M),poly2list(P,Ys),!.

%%%%%%%%%%%%%%%%



%%%%% 2 %%%%%%%%

simpoly_list(L,R):-acc(L,[],R),!.

acc([H|T],S,Y):-monom_add_polynom(H,S,[Z|Zs]),!,monom_add_polynom(Z,Zs,M),acc(T,M,Y),!.
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