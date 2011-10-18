-define(ETO(Type),
  #seto_payload{eto_payload=#eto_payload{type=Type}}).
-define(ETO(Type, Payload),
  #seto_payload{eto_payload=#eto_payload{type=Type, Type=Payload}}).

-define(SETO(Type),
  #seto_payload{type=Type}).
-define(SETO(Type, Payload),
  #seto_payload{type=Type, Type=Payload}).
