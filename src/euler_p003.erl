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
-export([is_prime/1, primes/1, max_prime/1, max_prime_factor/1]).

is_prime(X) when X < 2 ->
    false;
is_prime(X) ->
    X =:= max_prime(X).

primes(Limit) when Limit < 2 ->
    [];
primes(Limit) ->
    primes([2], lists:seq(3,Limit,2)).

primes(Primes, []) ->
    lists:reverse(Primes);
primes([Prime|Primes], Integers) ->
    [NextPrime | NextIntegers] = [X || X <- Integers, X rem Prime =/= 0 ],
    primes([NextPrime, Prime | Primes], NextIntegers).

max_prime(Limit) ->
    max_prime(2, lists:seq(3,Limit,2)).

max_prime(Prime, []) ->
    Prime;
max_prime(Prime, Integers) ->
    [NextPrime | NextIntegers] = [X || X <- Integers, X rem Prime =/= 0 ],
    max_prime(NextPrime, NextIntegers).

max_prime_factor(N) when N < 2 ->
    1;
max_prime_factor(N) ->
    Limit = trunc(math:sqrt(N)) + 1,
    IsFactor = fun(X) -> N rem X =:= 0 end,
    max_prime_if(Limit, IsFactor).

max_prime_if(Limit, IsFactor) ->
    max_prime_if(IsFactor, 1, 2, lists:seq(3, Limit, 2)).

max_prime_if(_IsFactor, Acc, _Prime, []) ->
    Acc;
max_prime_if(IsFactor, Acc, Prime, Integers) ->
    [NextPrime | NextIntegers] = [X || X <- Integers, X rem Prime =/= 0],
    case IsFactor(NextPrime) of
        true -> max_prime_if(IsFactor, NextPrime, NextPrime, NextIntegers);
        false -> max_prime_if(IsFactor, Acc, NextPrime, NextIntegers)
    end.
