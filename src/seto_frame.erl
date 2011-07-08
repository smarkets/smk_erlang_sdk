-module(seto_frame).

-export([deframe/2, frame/1]).

deframe(<<Type:1, Bytes:7/integer, Rest/binary>> = All) ->
  case Rest of
    <<MsgData:Bytes/binary-unit:8, Buf/binary>> ->
      {Type, MsgData, Buf};
    _ ->
      All
  end;
deframe(Data) ->
  Data.

deframe(Data, Buf) ->
  deframe(<<Buf/binary, Data/binary>>).

frame(IOList) ->
  Bin = iolist_to_binary(IOList),
  Bytes = byte_size(Bin),
  if
    Bytes > 127 ->
      {error, too_big};
    true ->
      <<0:1, Bytes:7/integer, Bin:Bytes/binary-unit:8>>
  end.
