-module(smk_eto_frame).

-export([buf/0, buf_append/2, deframe/1, frame/1]).

-record(buf, {
    size = waiting,
    bytes = <<>>
  }).

-spec buf() -> #buf{}.
buf() -> #buf{}.

-spec buf_append(#buf{}, binary()) -> #buf{}.
buf_append(Buf, Data) ->
    Buf#buf{bytes = <<(Buf#buf.bytes)/binary, Data/binary>>}.

-spec deframe(binary() | #buf{}) -> #buf{} | {binary(), #buf{}}.
deframe(Bin) when is_binary(Bin) -> deframe(#buf{bytes = Bin});
deframe(#buf{bytes = <<>>} = Buf) -> Buf;
deframe(#buf{size = I, bytes = B} = Buf)
  when is_integer(I),
    (I > 2 andalso byte_size(B) >= I)
    orelse (I < 3 andalso byte_size(B) >= 3) ->
  Padding = max(4 - (I + 1), 0) * 8,
  <<MsgData:I/binary-unit:8, _:Padding, Rest/binary>> = B,
  {MsgData, Buf#buf{size = waiting, bytes = Rest}};
deframe(#buf{size = Cont, bytes = B} = Buf)
  when not is_integer(Cont) ->
  Sz = case Cont of
           waiting -> eto_uleb128:decode(B);
           _       -> eto_uleb128:decode(B, Cont)
       end,
  case Sz of
      I when is_integer(I) ->
          Buf#buf{size = I, bytes = <<>>};
      {I, Rest} when is_integer(I) ->
          deframe(Buf#buf{size = I, bytes = Rest});
      Cont1 ->
          Buf#buf{size = Cont1, bytes = <<>>}
  end;
deframe(Buf) -> Buf. % nothing to do

-spec frame(iolist()) -> binary().
frame(IOList) ->
  Bin = iolist_to_binary(IOList),
  Sz = byte_size(Bin),
  % Frames must be a minimum of 4 bytes including the size header
  Pad = max(4 - (Sz + 1), 0) * 8,
  <<(eto_uleb128:encode(Sz))/binary, Bin:Sz/binary-unit:8, 0:Pad>>.
