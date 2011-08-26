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

-define(assertRecv(Payload),
  Payload = recv()).

-define(assertEto(Eto),
  ?assertRecv(#seto_payload{
      type=eto,
      eto_payload=Eto
    })).

-define(assertLoginResponse(Seq),
  ?assertRecv(?loginResponse(Seq))).

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
