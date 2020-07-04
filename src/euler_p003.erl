%%% @author Alexandros Bantis <ambantis@gmail.com>
%%% @copyright (C) 2020, Alexandros Bantis
%%% @doc
%%% Largest prime factor
%%% Problem 3
%%%
%%% The prime factors of 13195 are 5, 7, 13 and 29.
%%%
%%% What is the largest prime factor of the number 600,851,475,143 ?
%%%
%%% https://projecteuler.net/problem=3
%%% @end
%%% Created : 16 May 2020 by Alexandros Bantis <ambantis@gmail.com>

-module(euler_p003).
-export([first_factor/1, is_prime/1, prime_factors/1, primes/1, max_prime_factor/1]).

is_prime(N) when N < 2 ->
    false;
is_prime(N) ->
    case first_factor(N) of
        nil -> true;
        X -> is_prime(N div X)
    end.

limit(X) ->
    trunc(math:sqrt(X)) + 1.

primes(Limit) when Limit < 2 ->
    [];
primes(Limit) ->
    primes([2], lists:seq(3,Limit,2)).

primes(Primes, []) ->
    lists:reverse(Primes);
primes([Prime|Primes], Integers) ->
    [NextPrime | NextIntegers] = [X || X <- Integers, X rem Prime =/= 0 ],
    primes([NextPrime, Prime | Primes], NextIntegers).

first_factor(N) ->
    first_factor(limit(N), 2, N).

first_factor(Limit, X, _N) when X > Limit ->
    nil;
first_factor(_Limit, X, N) when N rem X =:= 0 ->
    X;
first_factor(Limit, X, N) ->
    first_factor(Limit, X + 1, N).

prime_factors(N) when N < 1 ->
    1;
prime_factors(N) ->
    prime_factors([], N).

prime_factors(Acc, N) ->
    case first_factor(N) of
        nil -> [N | Acc];
        X -> prime_factors([X|Acc], N div X)
    end.

max_prime_factor(N) when N < 1 ->
    1;
max_prime_factor(N) ->
    case first_factor(N) of
        nil -> N;
        X -> max_prime_factor(N div X)
    end.
