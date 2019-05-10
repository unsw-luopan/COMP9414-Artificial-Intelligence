%Author:Pan Luo
%Student Number:z5192086
%COMP9414:Assignment 1 - Prolog Programming
%Complete Date:17/03/2019


%Q1,sumsq_even(Numbers, Sum)
%First, differentiate the even and odd number in the list.
%Then sum the squre of these even number and ignore the odd number in the sum.

sumsq_even([],0).
sumsq_even(Number, Sum):-
    [Head|Tail] = Number,
    sumsq_even(Tail,RestSum),
    integer(Head),
    0 =:= Head mod 2,
    Sum is Head*Head + RestSum.
sumsq_even(Number, Sum):-
    [Head|Tail] = Number,
    sumsq_even(Tail,RestSum),
    integer(Head),
    1 =:= Head mod 2,
    Sum is RestSum.




%Q2,same_name(Person1,Person2)
%Same name means Person1 and Person2 has the same male ancestor, or one of them is an ancestor of another,
%or they are one person.
%Use parent and Male to predicate father, then use father to predicate maleAncestor recursively.

father(Person1,Person2):-
    parent(Person1,Person2),
    male(Person1).

maleAncestor(Person1,Person2):- 
    Person1 = Person2.
maleAncestor(Person1,Person2):-
    father(Person3,Person2),
    maleAncestor(Person1,Person3).

same_name(Person1,Person2):-
    maleAncestor(Person1,Person2).
same_name(Person1,Person2):-
    maleAncestor(Person2,Person1).
same_name(Person1,Person2):-
    maleAncestor(X,Person1),
    maleAncestor(X,Person2).




%Q3,sqrt_list(NumberList, ResultList)
%Use built-in sqrt() to calculate the root of a number in the list.

sqrt_list([],[]).
sqrt_list(NumberList, ResultList):-
    [Head|Tail] = NumberList,
    [[Head,Root]|Rest]=ResultList,
    Head >= 0,
    Root is sqrt(Head),
    sqrt_list(Tail,Rest).




%Q4,sign_runs(List, RunList)
%First,I write a concat predicate to concat two list.
%In order to differentiate the positive and nagetive number, kick head of a list firstly, and judge the whether
%the next element has the same sign with the head, then do it recursively.
%If one element has different sign with the previous element(s),then kick this one as the new head of a list, then do 
%step 1 recursively.


concat([], List2, List2).
concat([Item | Tail1], List2, [Item | Concat_Tail1_List2]) :-
      concat(Tail1, List2, Concat_Tail1_List2).

positiveHead([], [], []).
positiveHead([Head|Rest], Result, RestList) :- 
    Head>=0,
    positiveHead(Rest, RestResult, RestList),
    concat([Head],RestResult,Result).
positiveHead([Head|Rest], Result, RestList) :- 
    Head<0,
    RestList = [Head|Rest],
    Result = [].


negativeHead([], [], []).
negativeHead([Head|Rest], Result, RestList) :- 
    Head<0,
    negativeHead(Rest, RestResult, RestList),
    concat([Head],RestResult,Result).
negativeHead([Head|Rest], Result, RestList) :- 
    Head>=0,
    RestList = [Head|Rest],
    Result = [].

sign_runs([],[]).
sign_runs(List, RunList):-
    [Head|_] = List,
    Head >= 0,
    positiveHead(List, PositiveResult, RestList),
   sign_runs(RestList, NewList),
    [PositiveResult|NewList] = RunList.
sign_runs(List,RunList):-
    [Head|_] = List,
    Head<0,
    negativeHead(List, NegativeResult, RestList),
    sign_runs(RestList, NewList),
    [NegativeResult|NewList] = RunList.



%Q5£¬is_heap(Tree)
%Disscuss 5 different cases where
%The whole tree is empty.
%Both left hand side and right hand side are empty.
%LFS is empty and RHS is a tree.
%RHS is empty and LFS is a tree.
%Both LFS and RHS are not empty.


is_heap(empty).
is_heap(tree(empty,_,empty)).
is_heap(tree(tree(LeftNode,LeftRoot,RightNode), Root, empty)) :-
    LeftRoot >= Root,
    is_heap(tree(LeftNode,LeftRoot,RightNode)).
is_heap(tree(empty, Root, tree(LeftNode,RightRoot,RightNode))) :-
    RightRoot >= Root,
    is_heap(tree(LeftNode,RightRoot,RightNode)).
is_heap(tree(tree(LeftNode,LeftRoot,RightNode), Root, tree(LeftNode,RightRoot,RightNode))) :-
    LeftRoot >= Root,
    RightRoot >= Root,
    is_heap(tree(LeftNode,LeftRoot,RightNode)),
    is_heap(tree(LeftNode,RightRoot,RightNode)).












