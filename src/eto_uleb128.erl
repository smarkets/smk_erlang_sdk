-module(eto_uleb128).

-export([encode/1, encode/2, decode/1, decode/2]).

% For property tests
-include_lib("proper/include/proper.hrl").
-export([prop_encode_decode/0]).

-spec encode(non_neg_integer()) -> binary().
encode(I) when I >= 0 -> encode_1(I, 0).

-spec encode(non_neg_integer(), binary()) -> binary().
encode(I, Acc) -> <<Acc/binary, (encode_1(I, 0))/binary>>.

-spec encode_1(non_neg_integer(),
               non_neg_integer()) -> binary().
encode_1(I, Acc) when I > 16#7f ->
    Val = I band 16#7f bor 16#80,
    encode_1(I bsr 7, Acc bsl 8 bor Val);
encode_1(I, Acc) ->
    binary:encode_unsigned(Acc bsl 8 bor I).

-type decode_cont() :: {cont,
                        non_neg_integer(),
                        non_neg_integer(),
                        non_neg_integer()}.
-type decode_ret() :: non_neg_integer()
                      | {non_neg_integer(), binary()}
                      | decode_cont().
-spec decode(binary()) -> decode_ret().
decode(<<0:1, A:7>>) -> A;
decode(<<0:1, A:7, Rest/binary>>) -> {A, Rest};
decode(<<1:1, A:7, 0:1, B:7>>) -> B bsl 7 bor A;
decode(<<1:1, A:7, 0:1, B:7, Rest/binary>>) ->
    {B bsl 7 bor A, Rest};
decode(<<1:1, A:7, 1:1, B:7, 0:1, C:7>>) ->
    (C bsl 14) bor (B bsl 7) bor A;
decode(<<1:1, A:7, 1:1, B:7, 0:1, C:7, Rest/binary>>) ->
    {(C bsl 14) bor (B bsl 7) bor A, Rest};
decode(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 0:1, D:7>>) ->
    (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A;
decode(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 0:1, D:7, Rest/binary>>) ->
    {(D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A, Rest};
decode(<<1:1, A:7, 1:1, B:7, 1:1, C:7, 1:1, D:7, _/binary>> = Bin) ->
    Acc = (D bsl 21) bor (C bsl 14) bor (B bsl 7) bor A,
    decode(Bin, {cont, Acc, 4, 4});
decode(Bin) ->
    decode(Bin, {cont, 0, 0, 0}).

-spec decode(binary(), decode_cont()) -> decode_ret().
decode(Bin, {cont, Acc, X, Offset}) ->
    case Bin of
        <<_:Offset/bytes, 0:1, I:7>> ->
            Acc bor (I bsl (X * 7));
        <<_:Offset/bytes, 0:1, I:7, Rest/binary>> ->
            Result = Acc bor (I bsl (X * 7)),
            {Result, Rest};
        <<_:Offset/bytes, 1:1, I:7>> ->
            {cont, Acc bor (I bsl (X * 7)), X + 1, 0};
        <<_:Offset/bytes, 1:1, I:7, _/binary>> ->
            Acc1 = Acc bor (I bsl (X * 7)),
            decode(Bin, {cont, Acc1, X + 1, Offset + 1})
    end.

large_non_neg_integer() ->
    union([integer(0, inf),
           integer(16#7f, inf),
           integer(16#3fff, inf),
           integer(16#1fffff, inf),
           integer(16#fffffff, inf)]).

% Properties for testing
prop_encode_decode() ->
    ?FORALL(I, large_non_neg_integer(), begin I =:= decode(encode(I)) end).

prop_encode_decode_long() ->
    ?FORALL(I, large_non_neg_integer(),
            begin
                Bin = encode(I),
                Cont = decode(<<(binary:at(Bin, 0))>>),
                I =:=
                    lists:foldl(
                      fun(X, Acc) ->
                              decode(<<(binary:at(Bin, X))>>, Acc)
                      end, Cont, lists:seq(1, byte_size(Bin) - 1))
            end).
