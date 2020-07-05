%%% @author Alexandros Bantis <ambantis@gmail.com>
%%% @copyright (C) 2020, Alexandros Bantis
%%% @doc
%%%
%%% Largest palindrome product
%%% Problem 4
%%%
%%% A palindromic number reads the same both ways. The largest palindrome made
%%% from the product of two 2-digit numbers is 9009 = 91 × 99.
%%%
%%% Find the largest palindrome made from the product of two 3-digit numbers.
%%%
%%% https://projecteuler.net/problem=4
%%% @end
%%% Created : 4 July 2020 by Alexandros Bantis <ambantis@gmail.com>

-module(euler_p004).

-export([largest_palindrome_product/1]).

%%-----------------------------------------------------------------------------
%% @doc Returns tuple of two N-Digit numbers that evaluate to largest
%% palindrome.
%%
%% @end
%%-----------------------------------------------------------------------------
largest_palindrome_product(Digits) when is_integer(Digits) ->
    {Lo, Hi} = range(Digits),
    Ceiling = Hi * Hi,
    largest_palindrome_product(Ceiling, {Lo, Hi}).

largest_palindrome_product(N, {Lo, Hi}) ->
    case is_palindrome(N) of
      true ->
          case find_multiple_pair(N, Hi, Lo) of
            {none} ->
                largest_palindrome_product(N - 1, {Lo, Hi});
            {some, {X, Y}} ->
                {X, Y}
          end;
      false ->
          largest_palindrome_product(N - 1, {Lo, Hi})
    end.

find_multiple_pair(_Of, N, Lo) when N < Lo ->
    {none};
find_multiple_pair(Of, N, Lo) when Of rem N == 0 ->
    X = N,
    Y = Of div X,
    case in_bounds(Y, {Lo, X}) of
      true ->
          {some, {X, Y}};
      false ->
          find_multiple_pair(Of, N - 1, Lo)
    end;
find_multiple_pair(Of, N, Lo) ->
    find_multiple_pair(Of, N - 1, Lo).

in_bounds(N, {Lo, Hi}) ->
    Lo =< N andalso N =< Hi.

range(Digits) when is_integer(Digits) ->
    Min = [$1 | string:chars($0, Digits - 1)],
    Max = string:chars($9, Digits),
    {list_to_integer(Min), list_to_integer(Max)}.

is_palindrome(Number) when is_integer(Number) ->
    Forward = integer_to_list(Number),
    Reverse = lists:reverse(Forward),
    is_palindrome(Forward, Reverse).

is_palindrome([], []) ->
    true;
is_palindrome([], _Reverse) ->
    false;
is_palindrome(_Forward, []) ->
    false;
is_palindrome([FH | FTail], [RH | RTail]) ->
    case FH =:= RH of
      true ->
          is_palindrome(FTail, RTail);
      false ->
          false
    end.
