-include_lib("eunit/include/eunit.hrl").
-include("eto_piqi.hrl").
-include("seto_piqi.hrl").

recv() ->
  receive
    Any ->
      #seto_payload{type=Type, eto_payload=#eto_payload{seq=Seq}} = Any,
      io:format("Recevied ~p~n", [Seq]),
      case Type of
        order_accepted ->
          #seto_payload{order_accepted=A} = Any,
          io:format("Accepted ~p~n", [A]);
          _ -> ok
      end,
      Any
  end.

-spec all(list(any()), fun((any(), any()) -> any())) -> list(any()).
all([], _) -> [];
all(L, F)  -> all(L, F, recv(), [], []).

-spec all(list(any()), fun((any(), any()) -> any()), '%all' | seto_payload(),
      list(any()), list(any())) -> list(any()).
all([], _, '$all', [], Acc) ->
  Acc;
all(L, F, '$all', L2, Acc) when L =/= [] orelse L2 =/= [] ->
  all(L++L2, F, recv(), [], Acc);
all([], F, Recv, [H], Acc) ->
  Ret = F(H, Recv), % none left, definitely not one of these
  [Ret|Acc];
all([], F, Recv, L, Acc) ->
  ?debugFmt("all([], F, ~p, ~p, ~p)", [Recv, L, Acc]),
  all(L, F, Recv, [], Acc);
all([H|T], F, Recv, Tried, Acc) ->
  try
    Ret = F(H, Recv),
    ?debugFmt("match ~p ~p", [H, Recv]),
    all(T ++ Tried, F, '$all', [], [Ret|Acc])
  catch
    error:{badmatch,Recv} ->
      ?debugFmt("badmatch ~p ~p", [H, Recv]),
      all(T, F, Recv, [H|Tried], Acc)
  end.

-define(assertSeq(Seq),  ?assertEto(#eto_payload{seq=Seq})).

-define(loginResponse(Seq), 
  #seto_payload{
    eto_payload=#eto_payload{
      seq=Seq,
      type=login_response,
      is_replay=false,
      login_response=#eto_login_response{}
    }
  }).
-define(loginResponse(Seq, Session), 
  #seto_payload{
    eto_payload=#eto_payload{
      seq=Seq,
      type=login_response,
      is_replay=false,
      login_response=#eto_login_response{
        session=Session
      }
    }
  }).
-define(assertRecv(Payload),
  Payload = recv()).

-define(assertEto(Eto),
  ?assertRecv(#seto_payload{
      type=eto,
      eto_payload=Eto
    })).

-define(assertLoginResponse(Seq),
  ?assertRecv(?loginResponse(Seq))).

-define(assertLoginResponse(Seq, Session),
  ?assertRecv(?loginResponse(Seq, Session))).

-define(assertPong(Seq),
  ?assertEto(#eto_payload{
      type=pong,
      seq=Seq
    })).

-define(assertPongReplay(Seq),
  ?assertEto(#eto_payload{
      type=pong,
      seq=Seq,
      is_replay=true
    })).

-define(assertGapfill(Seq),
  ?assertEto(#eto_payload{
      type=gapfill,
      seq=Seq,
      is_replay=true
    })).

-define(assertLogoutConfirmation(Seq),
  ?assertLogout(Seq, confirmation)).

-define(assertAccountState(Seq, State),
  ?assertRecv(#seto_payload{
      eto_payload=#eto_payload{seq=Seq},
      type=account_state,
      account_state=State
    })).

-define(assertLogout(Seq, Reason),
  ?assertRecv(#seto_payload{
      eto_payload=#eto_payload{
        seq=Seq,
        type=logout,
        logout=#eto_logout{
          reason=Reason
        }
      }
    })).

-define(orderAccepted(Seq, Order, ResponseSeq),
    #seto_payload{
      type=order_accepted,
      order_accepted=#seto_order_accepted{
        order=Order,
        seq=ResponseSeq
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    }).

-define(assertOrderAccepted(Seq, Order, ResponseSeq),
  ?assertRecv(?orderAccepted(Seq, Order, ResponseSeq))).

-define(assertOrderRejected(Seq, Reason, ResponseSeq),
  ?assertRecv(#seto_payload{
      type=order_rejected,
      order_rejected=#seto_order_rejected{
        reason=Reason,
        seq=ResponseSeq
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).

-define(assertOrderCancelled(Seq, Order, Reason),
  ?assertRecv(?orderCancelled(Seq, Order, Reason))).

-define(orderCancelled(Seq, Order, Reason),
  #seto_payload{
    type=order_cancelled,
    order_cancelled=#seto_order_cancelled{
      order=Order,
      reason=Reason
    },
    eto_payload=#eto_payload{
      seq=Seq
    }
  }).


-define(assertOrderExecuted(Seq, Order, Qty, Px),
  ?assertRecv(#seto_payload{
      type=order_executed,
      order_executed=#seto_order_executed{
        order=Order,
        quantity=Qty,
        price=Px
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).

-define(assertContractQuotes(Seq, Contract),
  ?assertRecv(#seto_payload{
      type=contract_quotes,
      contract_quotes=#seto_contract_quotes{
        contract=Contract
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).
-define(assertMarketQuotes(Seq, Market),
  ?assertRecv(#seto_payload{
      type=market_quotes,
      market_quotes=#seto_market_quotes{
        market=Market
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).
