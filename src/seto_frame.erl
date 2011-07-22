-module(seto_frame).

-export([deframe/1, frame/2]).

deframe(Bin) -> deframe(Bin, []).
deframe(<<Type:1, Bytes:15/integer, Rest/binary>> = All, Acc) ->
  case Rest of
    <<MsgData:Bytes/binary-unit:8, Buf/binary>> ->
      deframe(Buf, {type_to_atom(Type), MsgData});
    _ ->
      {All, Acc}
  end;
deframe(All, Acc) ->
  {All, Acc}.

frame(Type, IOList) ->
  Bin = iolist_to_binary(IOList),
  Bytes = byte_size(Bin),
  if
    Bytes > 32767 ->
      {error, too_big};
    true ->
      <<(atom_to_type(Type)):1, Bytes:15/integer, Bin:Bytes/binary-unit:8>>
  end.

type_to_atom(0) -> eto;
type_to_atom(1) -> impl.
atom_to_type(eto) -> 0;
atom_to_type(impl) -> 1.
