-include("eto_piqi.hrl").
-include("seto_piqi.hrl").

recv() ->
  receive
    Any -> Any
  end.

-spec all(list(any()), function()) -> ok.
all([], _) -> ok;
all(L, F)  -> all(L, F, recv(), []).

-spec all(list(any()), function(), seto_payload(), list(any())) -> ok.
all([], F, Recv, [H|_]) ->
  F(H, Recv); % none left, definitely not one of these
all([H|T], F, Recv, Acc) ->
  try
    F(H, Recv),
    all(T, F)
  catch
    error:{badmatch,Recv} ->
      all(T, F, Recv, [H|Acc])
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

-define(assertOrderAccepted(Seq, Order, ResponseSeq),
  ?assertRecv(#seto_payload{
      type=order_accepted,
      order_accepted=#seto_order_accepted{
        order=Order,
        seq=ResponseSeq
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).

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
