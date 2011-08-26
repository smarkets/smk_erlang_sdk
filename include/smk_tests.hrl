recv() ->
  receive
    Any -> Any
  end.


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

-define(assertLogoutConfirmation(Seq),
  ?assertRecv(#seto_payload{
      eto_payload=#eto_payload{
        seq=Seq,
        type=logout,
        logout=#eto_logout{
          reason=confirmation
        }
      }
    })).

-define(assertOrderAccepted(Seq, OrderId, ResponseSeq),
  ?assertRecv(#seto_payload{
      type=order_accepted,
      order_accepted=#seto_order_accepted{
        order=OrderId,
        seq=ResponseSeq
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).

-define(assertOrderCancelled(Seq, OrderId, Reason),
  ?assertRecv(#seto_payload{
      type=order_cancelled,
      order_cancelled=#seto_order_cancelled{
        order=OrderId,
        reason=Reason
      },
      eto_payload=#eto_payload{
        seq=Seq
      }
    })).
