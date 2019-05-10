%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   agent.pl
%   Nine-Board Tic-Tac-Toe Agent
%   COMP9414 Artificial Intelligence, 2019 Term1
%   Group Homework
%   Group members: Shuxiang Zou (z5187969)
%                 Pan Luo (z5192086)
%
%  The functions and methods we finish this homework:
%    First, define some rules in the Nine-Board Tic-Tac-Toe game by using
%  functions called mark(+P,+M,+SubBoard0,-SubBoard1), play(+P,+L,+M,+Board0,-Board1),
%  legal(+L,-M,+Board), subwin(+P,SubBoard). These functions make sure that two
%  players follow the game rule such as the choosing the right subboard on next
%  move, check if the move is legal and so on. Then define the connection function
%  for playing on the server called connect(Port) to open socket and establish
%  TCP read/write streams.
%    The AI part is about search algorithm. We use negamax search and apply purning
%  by using functions called alpha_beta(+P,+D,+Board,+Alpha,+Beta,-Move,-Value)
%  and cutoff(+P,+Moves,+Board,+D,+Alpha,+Beta,+BestMove,+M,+V,-ChosenMove,-Value).
%  The search algorithm using alpha=-beta on some nodes of the tree. The way of
%  choosing these nodes is evaluating the subboard value by function called
%  value(P,_L,[A1,A2,A3,A4,A5,A6,A7,A8,A9],Value). This value function will
%  calculate each subboard value and sum them together to get a final total
%  value used in the tree search. Each subboard value will be calculate by the
%  function called eachV(P,SubBoard,Value). We calculate each subboard value by
%  using f(n)=3x2+x1-3O2-O1. And we enumerate all the possiable situations in
%  this function. Then the negamax search can run successfully. Finally, we apply
%  pruning by the function called
%  cutoff(+P,+Moves,+Board,+D,+Alpha,+Beta,+BestMove,+M,+V,-ChosenMove,-Value).
%  This function will purn nodes which the value(the value here is the total
%  value of the whole nine boards) is greater than beta.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
other(x,o).
other(o,x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  mark(+P,+M,+SubBoard0,-SubBoard1)
%  mark move M for player P on SubBoard0 to produce SubBoard1
%
mark(P,1,[e|T],[P|T]).
mark(P,2,[A,e|T],[A,P|T]).
mark(P,3,[A,B,e|T],[A,B,P|T]).
mark(P,4,[A,B,C,e|T],[A,B,C,P|T]).
mark(P,5,[A,B,C,D,e|T],[A,B,C,D,P|T]).
mark(P,6,[A,B,C,D,E,e|T],[A,B,C,D,E,P|T]).
mark(P,7,[A,B,C,D,E,F,e|T],[A,B,C,D,E,F,P|T]).
mark(P,8,[A,B,C,D,E,F,G,e,I],[A,B,C,D,E,F,G,P,I]).
mark(P,9,[A,B,C,D,E,F,G,H,e],[A,B,C,D,E,F,G,H,P]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  play(+P,+L,+M,+Board0,-Board1)
%  mark move M for player P on board L of Board0 to produce Board1
%
play(_P,_L,0,Board,Board).
play(P,1,M,[B|T],[B1|T]) :- mark(P,M,B,B1).
play(P,2,M,[C,B|T],[C,B1|T]) :- mark(P,M,B,B1).
play(P,3,M,[C,D,B|T],[C,D,B1|T]) :- mark(P,M,B,B1).
play(P,4,M,[C,D,E,B|T],[C,D,E,B1|T]) :- mark(P,M,B,B1).
play(P,5,M,[C,D,E,F,B|T],[C,D,E,F,B1|T]) :- mark(P,M,B,B1).
play(P,6,M,[C,D,E,F,G,B|T],[C,D,E,F,G,B1|T]) :- mark(P,M,B,B1).
play(P,7,M,[C,D,E,F,G,H,B|T],[C,D,E,F,G,H,B1|T]) :- mark(P,M,B,B1).
play(P,8,M,[C,D,E,F,G,H,I,B,K],[C,D,E,F,G,H,I,B1,K]) :- mark(P,M,B,B1).
play(P,9,M,[C,D,E,F,G,H,I,J,B],[C,D,E,F,G,H,I,J,B1]) :- mark(P,M,B,B1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  empty(-M,+SubBoard)
%  check that cell M of SubBoard is empty
%
empty(1,[e|_]).
empty(2,[_,e|_]).
empty(3,[_,_,e|_]).
empty(4,[_,_,_,e|_]).
empty(5,[_,_,_,_,e|_]).
empty(6,[_,_,_,_,_,e|_]).
empty(7,[_,_,_,_,_,_,e|_]).
empty(8,[_,_,_,_,_,_,_,e,_]).
empty(9,[_,_,_,_,_,_,_,_,e]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  legal(+L,-M,+Board)
%  true if cell M of board L is legal
%
legal(1,M,[B|_]) :- empty(M,B).
legal(2,M,[_,B|_]) :- empty(M,B).
legal(3,M,[_,_,B|_]) :- empty(M,B).
legal(4,M,[_,_,_,B|_]) :- empty(M,B).
legal(5,M,[_,_,_,_,B|_]) :- empty(M,B).
legal(6,M,[_,_,_,_,_,B|_]) :- empty(M,B).
legal(7,M,[_,_,_,_,_,_,B|_]) :- empty(M,B).
legal(8,M,[_,_,_,_,_,_,_,B,_]) :- empty(M,B).
legal(9,M,[_,_,_,_,_,_,_,_,B]) :- empty(M,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  subwin(+P,SubBoard)
%  true if player P has achieved 3-in-a-row
%
subwin(P,[P,P,P|_]).
subwin(P,[_,_,_,P,P,P|_]).
subwin(P,[_,_,_,_,_,_,P,P,P]).
subwin(P,[P,_,_,P,_,_,P,_,_]).
subwin(P,[_,P,_,_,P,_,_,P,_]).
subwin(P,[_,_,P,_,_,P,_,_,P]).
subwin(P,[P,_,_,_,P,_,_,_,P]).
subwin(P,[_,_,P,_,P,_,P,_,_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  winning(+P,+L,+Board)
%  true if player P has achieved 3-in-a-row on board L
%
winning(P,1,[B|_]) :- subwin(P,B).
winning(P,2,[_,B|_]) :- subwin(P,B).
winning(P,3,[_,_,B|_]) :- subwin(P,B).
winning(P,4,[_,_,_,B|_]) :- subwin(P,B).
winning(P,5,[_,_,_,_,B|_]) :- subwin(P,B).
winning(P,6,[_,_,_,_,_,B|_]) :- subwin(P,B).
winning(P,7,[_,_,_,_,_,_,B|_]) :- subwin(P,B).
winning(P,8,[_,_,_,_,_,_,_,B,_]) :- subwin(P,B).
winning(P,9,[_,_,_,_,_,_,_,_,B]) :- subwin(P,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  open socket and establish TCP read/write streams
%
connect(Port) :-
   tcp_socket(Socket),
   gethostname(Host),
   tcp_connect(Socket,Host:Port),
   tcp_open_socket(Socket,INs,OUTs),
   assert(connectedReadStream(INs)),
   assert(connectedWriteStream(OUTs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  read next command and execute it
%
ttt :-
   connectedReadStream(IStream),
   read(IStream,Command),
   Command.

init :- ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  start(+P)
%  start a new game for player P
%
start(P) :-
   retractall(board(_ )),
   retractall(player(_ )),
   retractall(prev_move(_ )),
   assert(board(
   [[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],
    [e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],
    [e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e],[e,e,e,e,e,e,e,e,e]])),
   assert(player(P)),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  second_move(+K,+L)
%  assume first move is board K, cell L
%  choose second move and write it
%
second_move(K,L) :-
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   search(P,L,Board1,M),
   play(P,L,M,Board1,Board2),
   print_board(Board2),
   assert(board(Board2)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  third_Move(+J,+K,+L)
%  assume first move is board K, cell L,
%           second move is board L, cell M
%  choose third move and write it
%
third_move(J,K,L) :-
   retract(board(Board0)),
   player(P),
   play(P,J,K,Board0,Board1),
   print_board(Board1),
   other(P,Q),
   play(Q,K,L,Board1,Board2),
   print_board(Board2),
   search(P,L,Board2,M),
   play(P,L,M,Board2,Board3),
   print_board(Board3),
   assert(board(Board3)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  next_move(+L)
%  assume opponent move is L
%  choose (our) next move and write it
%
next_move(L) :-
   retract(prev_move(K)),
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   search(P,L,Board1,M),
   play(P,L,M,Board1,Board2),
   print_board(Board2),
   assert(board(Board2)),
   assert(prev_move(M)),
   write_output(M),
   ttt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  search(+P,+L,+Board,-M)
%  choose Move M for player P on board L
%
search(P,L,Board,Move) :-
   alpha_beta(P,L,6,Board,-2000,2000,Move,_Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  alpha_beta(+P,+D,+Board,+Alpha,+Beta,-Move,-Value)
%  perform alpha-beta search to depth D for player P,
%  assuming P is about to move on Board. Return Value
%  of current Board position, and best Move for P.


% if other player has won, Value is -1000
alpha_beta(P, _L,_D,Board,_Alpha,_Beta,0,-1000) :-
   other(P,Q),
   winning(Q,_,Board), ! .

% if we win, Value is 1000
alpha_beta(P, _L,_D,Board,_Alpha,_Beta,0,1000) :-
winning(P,_,Board), ! .

% if depth limit exceeded, use heuristic estimate
alpha_beta(P,L,0,Board,_Alpha,_Beta,0,Value) :-
   value(P,L,Board,Value), ! .

% evaluate and choose all legal moves in this position
alpha_beta(P,L,D,Board,Alpha,Beta,Move,Value) :-
   D > 0,
   findall(M,legal(L,M,Board),Moves),
   Moves \= [], !,
   Alpha1 is -Beta,
   Beta1 is -Alpha,
   D1 is D-1,
   eval_choose(P,L,Moves,Board,D1,Alpha1,Beta1,0,Move,Value).

% if no available moves, it must be a draw
alpha_beta(_P,_L,_D,_Board,_Alpha,_Beta,0,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   value(P,_L,[A1,A2,A3,A4,A5,A6,A7,A8,A9],Value)
%   Calculate the total value of all SubBoards
%   Player P play on board L and its score is Value.
%
value(P,_L,[A1,A2,A3,A4,A5,A6,A7,A8,A9],Value):-
  eachV(P,A1,ValueA1),
  eachV(P,A2,ValueA2),
  eachV(P,A3,ValueA3),
  eachV(P,A4,ValueA4),
  eachV(P,A5,ValueA5),
  eachV(P,A6,ValueA6),
  eachV(P,A7,ValueA7),
  eachV(P,A8,ValueA8),
  eachV(P,A9,ValueA9),
  Value = ValueA1+ValueA2+ValueA3+ValueA4+ValueA5+ValueA6+ValueA7+ValueA8+ValueA9.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   eachV(P,SubBoard,Value)
%   Calculate the SubBoard Value
%   The value is equal to the evaluation function f(n)=3X2+X1-3O2-O1
%
eachV(P,SubBoard,Value):-
   other(P,Q),
   findall(ScoreP,subboardenumerate(P,Q,SubBoard,ScoreP),ScoresP),
   sumlist(ScoresP,ValueP),
   findall(ScoreQ,subboardenumerate(Q,P,SubBoard,ScoreQ),ScoresQ),
   sumlist(ScoresQ,ValueQ),
   Value is ValueP-ValueQ.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Enumerate all the possiable situations based on the valus function f(n)=3X2+X1-3O2-O1
%

%two pieces are near each other on horizontal and the third in the row is empty
subboardenumerate(P,_,[P,P,e,_,_,_,_,_,_],3).
subboardenumerate(P,_,[P,e,P,_,_,_,_,_,_],3).
subboardenumerate(P,_,[e,P,P,_,_,_,_,_,_],3).
subboardenumerate(P,_,[_,_,_,P,P,e,_,_,_],3).
subboardenumerate(P,_,[_,_,_,P,e,P,_,_,_],3).
subboardenumerate(P,_,[_,_,_,e,P,P,_,_,_],3).
subboardenumerate(P,_,[_,_,_,_,_,_,P,P,e],3).
subboardenumerate(P,_,[_,_,_,_,_,_,P,e,P],3).
subboardenumerate(P,_,[_,_,_,_,_,_,e,P,P],3).

%two pieces are near each other on vertical and the third in the row is empty
subboardenumerate(P,_,[P,_,_,P,_,_,e,_,_],3).
subboardenumerate(P,_,[P,_,_,e,_,_,P,_,_],3).
subboardenumerate(P,_,[e,_,_,P,_,_,P,_,_],3).
subboardenumerate(P,_,[_,P,_,_,P,_,_,e,_],3).
subboardenumerate(P,_,[_,P,_,_,e,_,_,P,_],3).
subboardenumerate(P,_,[_,e,_,_,P,_,_,P,_],3).
subboardenumerate(P,_,[_,_,P,_,_,P,_,_,e],3).
subboardenumerate(P,_,[_,_,P,_,_,e,_,_,P],3).
subboardenumerate(P,_,[_,_,e,_,_,P,_,_,P],3).

%two pieces are near each other on diagonal and the third in the diagonal is empty
subboardenumerate(P,_,[P,_,_,_,P,_,_,_,e],3).
subboardenumerate(P,_,[P,_,_,_,e,_,_,_,P],3).
subboardenumerate(P,_,[e,_,_,_,P,_,_,_,P],3).
subboardenumerate(P,_,[_,_,P,_,P,_,e,_,_],3).
subboardenumerate(P,_,[_,_,P,_,e,_,P,_,_],3).
subboardenumerate(P,_,[_,_,e,_,P,_,P,_,_],3).

%one pieces is in a board and horizontal line is empty
subboardenumerate(P,_,[P,e,e,_,_,_,_,_,_],1).
subboardenumerate(P,_,[e,P,e,_,_,_,_,_,_],1).
subboardenumerate(P,_,[e,e,P,_,_,_,_,_,_],1).
subboardenumerate(P,_,[_,_,_,P,e,e,_,_,_],1).
subboardenumerate(P,_,[_,_,_,e,P,e,_,_,_],1).
subboardenumerate(P,_,[_,_,_,e,e,P,_,_,_],1).
subboardenumerate(P,_,[_,_,_,_,_,_,P,e,e],1).
subboardenumerate(P,_,[_,_,_,_,_,_,e,P,e],1).
subboardenumerate(P,_,[_,_,_,_,_,_,e,e,P],1).

%one pieces is in a board and vertical line is empty
subboardenumerate(P,_,[P,_,_,e,_,_,e,_,_],1).
subboardenumerate(P,_,[e,_,_,P,_,_,e,_,_],1).
subboardenumerate(P,_,[e,_,_,e,_,_,P,_,_],1).
subboardenumerate(P,_,[_,P,_,_,e,_,_,e,_],1).
subboardenumerate(P,_,[_,e,_,_,P,_,_,e,_],1).
subboardenumerate(P,_,[_,e,_,_,e,_,_,P,_],1).
subboardenumerate(P,_,[_,_,P,_,_,e,_,_,e],1).
subboardenumerate(P,_,[_,_,e,_,_,P,_,_,e],1).
subboardenumerate(P,_,[_,_,e,_,_,e,_,_,P],1).

%one pieces is in a board and diagonal line is empty
subboardenumerate(P,_,[P,_,_,_,e,_,_,_,e],1).
subboardenumerate(P,_,[e,_,_,_,P,_,_,_,e],1).
subboardenumerate(P,_,[e,_,_,_,e,_,_,_,P],1).
subboardenumerate(P,_,[_,_,P,_,e,_,e,_,_],1).
subboardenumerate(P,_,[_,_,e,_,P,_,e,_,_],1).
subboardenumerate(P,_,[_,_,e,_,e,_,P,_,_],1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  eval_choose(+P,+Moves,+Board,+D,+Alpha,+Beta,+BestMove
%              -ChosenMove,-Value)
%  Evaluate list of Moves and determine Value of position
%  as well as ChosenMove for this Board position
% (taking account of current BestMove for this position)

% if no more Moves, BestMove becomes ChosenMove and Value is Alpha
eval_choose(_P,_L,[],_Board,_D,Alpha,_Beta,BestMove,BestMove,Alpha).

% evaluate Moves, find Value of Board Position, and ChosenMove for P
eval_choose(P,L,[M|Moves],Board,D,Alpha,Beta,BestMove,ChosenMove,Value) :-
   play(P,L,M,Board,Board1),
   other(P,Q),
   alpha_beta(Q,M,D,Board1,Alpha,Beta,_Move1,Value1),
   V is -Value1,
   cutoff(P,L, Moves,Board,D,Alpha,Beta,BestMove,M,V,ChosenMove,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  cutoff(+P,+Moves,+Board,+D,+Alpha,+Beta,+BestMove,+M,+V,
%              -ChosenMove,-Value)
%  Compare move M (with value V) to Alpha and Beta,
%  and compute Value and ChosenMove appropriately.

% cut off the search, ChosenMove is M and Value is V
cutoff(_P,_L,_Moves,_Board,_D,_Alpha,Beta,_Move0,M,V,M,V) :-
   V >= Beta.

% Alpha increases to V, BestMove is M, continue search
cutoff(P,L,Moves,Board,D,Alpha,Beta,_BestMove,M,V,ChosenMove,Value) :-
   Alpha < V, V < Beta,
   eval_choose(P,L,Moves,Board,D,V,Beta,M,ChosenMove,Value).

% keep searching, with same Alpha, Beta, BestMove
cutoff(P,L,Moves,Board,D,Alpha,Beta,BestMove,_M,V,ChosenMove,Value) :-
   V =< Alpha,
   eval_choose(P,L,Moves,Board,D,Alpha,Beta,BestMove,ChosenMove,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  write_output(+M)
%  transmit the chosen move (M)
%
write_output(M) :-
   connectedWriteStream(OStream),
   write(OStream,M),
   nl(OStream), flush_output(OStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_board()
%
print_board([A,B,C,D,E,F,G,H,I]) :-
   print3boards(A,B,C),
   write('------+-------+------'),nl,
   print3boards(D,E,F),
   write('------+-------+------'),nl,
   print3boards(G,H,I),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_board()
%
print3boards([A1,A2,A3,A4,A5,A6,A7,A8,A9],
             [B1,B2,B3,B4,B5,B6,B7,B8,B9],
             [C1,C2,C3,C4,C5,C6,C7,C8,C9]) :-
   print_line(A1,A2,A3,B1,B2,B3,C1,C2,C3),
   print_line(A4,A5,A6,B4,B5,B6,C4,C5,C6),
   print_line(A7,A8,A9,B7,B8,B9,C7,C8,C9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  print_line()
%
print_line(A,B,C,D,E,F,G,H,I) :-
   write(A),write(' '),write(B),write(' '),write(C),write(' | '),
   write(D),write(' '),write(E),write(' '),write(F),write(' | '),
   write(G),write(' '),write(H),write(' '),write(I),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  last_move(+L)
%
last_move(L) :-
   retract(prev_move(K)),
   retract(board(Board0)),
   player(P), other(P,Q),
   play(Q,K,L,Board0,Board1),
   print_board(Board1),
   ttt.

win(_)  :- write('win'), nl,ttt.
loss(_) :- write('loss'),nl,ttt.
draw(_) :- write('draw'),nl,ttt.

end :- halt.
