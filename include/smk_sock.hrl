-define(sock_data(T), is_tuple(T), tuple_size(T) =:= 3, element(1,T) =:= tcp; is_tuple(T), tuple_size(T) =:= 3, element(1,T) =:= ssl).
-define(sock_closed(T), is_tuple(T), tuple_size(T) =:= 2, element(1,T) =:= tcp_closed; is_tuple(T), tuple_size(T) =:= 2, element(1,T) =:= ssl_closed).
-define(sock_error(T), is_tuple(T), tuple_size(T) =:= 2, element(1,T) =:= tcp_error; is_tuple(T), tuple_size(T) =:= 2, element(1,T) =:= ssl_error).
