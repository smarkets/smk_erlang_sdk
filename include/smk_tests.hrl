recv() ->
  receive
    Any -> Any
  end.


-define(LOGIN_RESPONSE(Seq), 
  #seto_payload{
    eto_payload=#eto_payload{
      seq=1,
      type=login_response,
      is_replay=false,
      login_response=#eto_login_response{}
    }
  }).

-define(assertRecv(Payload),
  Payload = recv()).

-define(assertEto(Eto),
  ?assertRecv(#seto_payload{
    eto_payload=Eto
  })).

-define(assertLoginResponse(Seq),
  ?assertRecv(?LOGIN_RESPONSE(Seq))).

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
