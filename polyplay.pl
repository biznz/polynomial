use_module(library(ansi_term)).

zero(0)-->["zero"].

unit(1)-->["one"].
unit(2)-->["two"].
unit(3)-->["three"].
unit(4)-->["four"].
unit(5)-->["five"].
unit(6)-->["six"].
unit(7)-->["seven"].
unit(8)-->["eight"].
unit(9)-->["nine"].

tenth(10)-->["ten"].
tenth(11)-->["eleven"].
tenth(12)-->["twelve"].
tenth(13)-->["thirteen"].
tenth(14)-->["fourteen"].
tenth(15)-->["fifteen"].
tenth(16)-->["sixteen"].
tenth(17)-->["seventeen"].
tenth(18)-->["eighteen"].
tenth(19)-->["nineteen"].

denary(20)-->["twenty"].
denary(30)-->["thirty"].
denary(40)-->["fourty"].
denary(50)-->["fifty"].
denary(60)-->["sixty"].
denary(70)-->["seventy"].
denary(80)-->["eighty"].
denary(90)-->["ninety"].

hundred(100)-->["hundred"].


num(X)-->decimal(X).
num(R)-->["minus"],decimal(X),{R is -X}.
num(X)-->fl(X).

fl(W)-->decimal(X),["point"],decimal(Y),
	{
		atom_length(Y,A),
		decimal(A,M,[]),
		po(L,["ten","raised","to","minus"|M],[]),
		S is Y*L,
		W is X+S
	}.

decimal(X)-->zero(X).
decimal(X)-->unit(X).
decimal(X)-->tenth(X).
decimal(X)-->denary(X).
decimal(X)-->denary(Y),unit(W),{X is Y+W}.
decimal(X)-->hundreds(X).
decimal(X)-->hundreds(Y),["and"],unit(W),{X is Y+W}.
decimal(X)-->hundreds(Y),["and"],denary(W),{X is Y+W}.
decimal(X)-->hundreds(Y),["and"],tenth(W),{X is Y+W}.
decimal(X)-->hundreds(Y),["and"],denary(W),unit(Z),{A is Y+W,X is Z+A}.

hundreds(W)-->unit(X),hundred(Y),{W is X*Y}.
hundreds(W)-->tenth(X),hundred(Y),{W is X*Y}.


var(X)-->[W],{pvars(V),member(X,V),term_string(X,W)}.

store(X)-->(store_s(X);store_y(X)).

store_s(X)-->[W],{
	var(A,S,[]),
	var(B,U,[]),
	append([A],[B],M),
	atomic_list_concat(M,X),
	term_string(X,W),
	atom_length(X,2),
	\+ W = "as",\+ W = "is"
}. %,{atom_length(X,2)}.

store_y(X)-->[W],{
	var(A,S,[]),
	unit(B,U,[]),
	append([A],[B],M),
	atomic_list_concat(M,X),
	term_string(X,W),
	atom_length(X,2),
	\+ W = "as",\+ W = "is"
}. 


assignment(X,Y,Result):-term_string(X,W),term_string(Y,Z),S = [W," = ",Z],atomic_list_concat(S,Result).

poly(X)-->mon(X).
poly(X+Y)-->mon(X),["plus"],poly(Y).
poly(X-Y)-->mon(X),["minus"],poly(Y).


mon(X)-->num(X).
mon(X)-->po(X).
mon(X*Y)-->num(X),["times"],po(Y).
mon(X*Y)-->num(X),po(Y).
mon(R)-->["minus"],po(X),{R = -X}.



po(X)-->var(X).
po(X^2)-->var(X),["squared"].
po(X^Y)-->var(X),["raised","to"],num(Y).
po(W)-->["ten","raised","to","minus"],decimal(Y),{W is (1/10)^(Y)}.


text2poly(S,Poly):-tokenize(S,L),poly(Poly,L,[]).

%% remove parentheses - fix for polynoms of the form  Mon Op Mon Op Poly %%
%% since they get built as Mon op (Mon Op Poly)

fix_poly(Y,H):-term_string(Y,L),
	split_string(L,"(","(",B),
	atomic_list_concat(B,S),
	split_string(S,")",")",M),
	atomic_list_concat(M,G),
	term_string(H,G).


tokenize(S,L):-split_string(S," "," ",L).


do_something(Input,Result):-tokenize(Input,L),instruction(Result,L,[]).

instruction(Result)-->["add"],poly(X),["with"],poly(Y),{fix_poly(X,A),fix_poly(Y,W),addpoly(W,A,Result)}.
instruction(Result)-->["subtract"],poly(X),["with"],poly(Y),{fix_poly(X,A),fix_poly(Y,W),scalepoly(A,-1,S),addpoly(S,W,Result)}.
instruction(Result)-->["simplify"],poly(X),{fix_poly(X,A),simpoly(A,Result)}.
instruction(Result)-->["multiply"],num(X),["with"],poly(Y),{fix_poly(Y,A),scalepoly(A,X,Result)}.

instruction(Result)-->["add"],poly(X),store(D),["with"],poly(Y),store(B),["as"],store(C),
	{fix_poly(X,A),fix_poly(Y,W),addpoly(W,A,J),assignment(C,J,Result)}.
instruction(Result)-->["subtract"],poly(X),store(D),["with"],poly(Y),store(B),["as"],store(C),
	{fix_poly(X,A),fix_poly(Y,W),scalepoly(A,-1,S),addpoly(S,W,J),assignment(C,J,Result)}.
instruction(Result)-->["simplify"],poly(X),["as"],store(C),
	{fix_poly(X,A),simpoly(A,J),assignment(C,J,Result)}.
instruction(Result)-->["multiply"],num(X),store(D),["with"],poly(Y),store(B),["as"],store(C),
	{fix_poly(Y,A),scalepoly(A,X,J),assignment(C,J,Result)}.


instruction(Result)-->["add"],{error(Result,["add"],[])}.
instruction(Result)-->["add"],[X|T],{error(Result,["add"],[])}.

instruction(Result)-->["subtract"],{error(Result,["subtract"],[])}.
instruction(Result)-->["subtract"],[X|T],{error(Result,["subtract"],[])}.

instruction(Result)-->["multiply"],{error(Result,["multiply"],[])}.
instruction(Result)-->["multiply"],[X|T],{error(Result,["multiply"],[])}.

instruction(Result)-->["simplify"],{error(Result,["simplify"],[])}.
instruction(Result)-->["simplify"],[X|T],{error(Result,["simplify"],[])}.


instruction(Result)-->["subtract"],{error(Result,["subtract"],[])}.
instruction(Result)-->["simplify"],{error(Result,["simplify"],[])}.
instruction(X)-->commands(X).


commands(W)-->["exit"],{W = "\nbye\n\n"}.
commands(W)-->["help"],
	{W = "List of available commands:\n
	add X with Y - adds polynomial X to polynomial Y - performs X+Y
	subtract X with Y - subtracts polynomial X to polynomial Y - performs -X+Y
	simplify X - simplifies polynomial X - performs X simplification
	multiply X with Y - multiply polynomial X with Y - performs X*Y (X must be a number)
	exit - exits the environment

	variables used in polynomials must be lower case characters,
	variables used to store polynomials can be of the for XY where
	X is a lower case character and Y is either a lower case character
	or a digit [1-9]"
	}.

commands(W)-->[H|T],{error(W,["general"],[])}.

error(Command)-->["general"],{Command = "Err!: Command not recognized\n\n \tType help for a list of available commands\n\tCommands should be followed by ENTER!!"}.
error(Command)-->["help"],{Command = "help  - calls helper text"}.

error(Command)-->["add"],{Command = "Err!: add command has the form add Poly with Poly"}.
error(Command)-->["subtract"],{Command = "Err!: subtract command has the form add Poly with Poly"}.
error(Command)-->["simplify"],{Command = "Err!: simplify command has the form simplify Poly"}.
error(Command)-->["multiply"],{Command = "Err!: multiply command has the form multiply scalar with Poly"}.

error(Command)-->["exit"],{Command = "exit - exits the environment"}.


exit("exit"):-writeText("\nbye\n\n"),!.


polyplay:-
		nl,write('> '),
		readText(Buffer),
		do_something(Buffer,Result),!,
		\+ exit(Buffer),!,
		writeText(Result),polyplay.

readText(String):-current_stream(0,_,C),read_string(C,"\n","\t\r",End,String).

writeText(String):-current_stream(1,_,C),write(C,String).


%%%%%%%%%%%%%%%%%%%%%%% ASSIGNMENT 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pvars([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

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

%operations on monomials,polynomials

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

poly2list(P-M,[C|Ys]):-monomial(M),delete_coefficient(-1*M,C),poly2list(P,Ys).


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
