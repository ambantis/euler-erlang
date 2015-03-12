%%%-------------------------------------------------------------------
%%% Multiples of 3 and 5
%%%
%%% If we list all the natural numbers below 10 that are multiples of 3 and 5, we get 3, 5, 6, 9.
%%% The sum of these multiples is 23. Find the sum of all the multiples of 3 and 5 below 1000.
%%%
%%% @author ambantis
%%% Created : 11. Mar 2015 7:36 AM
%%%-------------------------------------------------------------------
-module(euler_p001).
-author("ambantis").

%% API
-export([multiplesOf3And5/1]).

multiplesOf3And5(N) -> lists:sum([X || X <- naturalsTo(N), X rem 3 =:= 0 orelse X rem 5 =:= 0]).

naturalsTo(N) -> naturalsIter(N-2, [N-1]).

naturalsIter(0, [H|T]) -> [H|T];
naturalsIter(I, [H|T]) -> naturalsIter(I-1, [I|[H|T]]).


