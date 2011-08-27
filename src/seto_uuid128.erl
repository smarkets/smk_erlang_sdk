-module(seto_uuid128).

-include("seto_piqi.hrl").

-export([to_uuid128/1, from_uuid128/2]).
-export([to_int/1, from_int/1]).
-export([tag/1]).

tag(account) -> 44225;
tag(contract_group) -> 49188;
tag(contract) -> 52428;
tag(order) -> 65520;
tag(comment) -> 45476;
tag(entity) -> 1092;
tag(template) -> 1093;
tag(event) -> 4352;
tag(session) -> 39321;
tag(user) -> 3840;
tag(referrer) -> 20046.

to_uuid128(<<Uuid:28/binary, _:4/binary>>) ->
    I = erlang:list_to_integer(binary_to_list(Uuid), 16),
    from_int(I).

from_int(I) ->
    Low = I band 16#ffffffffffffffff,
    High = (I bsr 64) band 16#ffffffffffffffff,
    High1 = case High of 0 -> undefined; _ -> High end,
    #seto_uuid_128{low = Low, high = High1}.

from_uuid128(Uuid, Tag) ->
    I = to_int(Uuid),
    <<(pad(to_hex(I), 28)):28/bytes, (pad(to_hex(tag(Tag)), 4)):4/bytes>>.

to_int(#seto_uuid_128{low = Low, high = High0}) ->
    High = case High0 of undefined -> 0; _ -> High0 end,
    (High bsl 64) bor Low.

pad(B, S) when is_binary(B), is_integer(S), size(B) >= S -> B;
pad(B, S) when is_binary(B), is_integer(S), size(B) > 0 ->
    pad(<<$0, B:(size(B))/bytes>>, S).

to_hex(0) ->
    <<"0">>;
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(Bin) when is_binary(Bin) ->
    to_hex(Bin, []).

to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1)|Acc]).

to_hex_int(0, Acc) ->
    list_to_binary(Acc);
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15)|Acc]).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.
