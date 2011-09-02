Welcome to the Smarkets.com Erlang Software Development Kit!

The API for Smarkets.com is based on a set of messages transported between Smarkets' servers and clients asyncronously in a specific sequence. Each incoming and outgoing message to and from Smarkets is assigned a incrementing sequence number starting at 1 and only resetting back to 1 if a reset/re-login message is received. A full description of the API and how to handle sessions and sequence incrementing / playback is located in the file named API in the same project.

# Using with your Application #

Add the following dependency to your [rebar](https://github.com/basho/rebar).config and have at it.

    {smk, "0.2.1", {git, "https://github.com/smarkets/smk_erlang_sdk.git", {tag, "v0.2.1"}}}

Take a look at [smk_client](https://github.com/smarkets/smk_erlang_sdk/blob/master/src/smk_client.erl) and [smk_my_callbacks](https://github.com/smarkets/smk_erlang_sdk/blob/master/src/smk_my_callbacks.erl) example.

*Requirements*: piqi 0.5.6 - the erlang piqi dependency for this is included in the main piqi repository so will be grabbed by rebar get-deps. Installation of piqi is outlined below.

# Building #

Message formats are defined using Piqi data definition files and can be found in the eto\_common and smk\_api\_common dependancies of this project fetched by running:

    ./rebar get-deps

To install the piqic command used to compile the fetched Piqi data definitions into Erlang code you can either follow the documentation on http://piqi.org/ or you can look in the fetched dependancy that has already been fetched for you:

    cd deps/piqi/
    less INSTALL

The resulting Erlang code is generated for you by piqic and will be in src/seto\_piqi.erl, include/seto\_piqi.hrl, src/eto\_piqi.erl, and include/eto\_piqi.hrl after running:

    ./rebar compile

An example client is available in src/smk\_example\_client.erl - to start it up simply crack open the Erlang shell and start typing:

    erl -pa ebin/ deps/*/ebin
    1> application:start(lager).
    2> application:start(smk).
    3> smk_my_callbacks:start(<<"name@email.com">>,<<"password">>).
    {ok, <0.38.0>}
    Received {seto_payload,eto,
                       {eto_payload,1,login_response,false,undefined,
                           undefined,
                           {eto_login_response,
                               <<"446d0756-3ef5-4a5a-a146-e1c4c6167c56">>,
                               2},
                           undefined} ... }
    4> rr(seto_piqi).
    [...]
    5> smk_client:order(smk_my_callbacks, 400000, 25, buy, #seto_uuid_128{low=122001}, #seto_uuid_128{low=175002}).
    {ok, 2}
    Received {seto_payload,order_accepted,
                        {eto_payload,2,undefined,false,undefined,undefined,
                            undefined,undefined},
                        undefined,undefined,undefined,
                        {seto_order_accepted,2,
                            {seto_uuid_128,74302962988791779,0}}...}
    6> smk_client:order_cancel(smk_my_callbacks, #seto_uuid_128{low=74302962988791779}).
    {ok, 3}
    Received {seto_payload,order_cancelled,
                        {eto_payload,3,undefined,false,undefined,undefined,
                            undefined,undefined},
                        undefined,undefined,undefined,undefined,undefined,
                        undefined,
                        {seto_order_cancelled,
                            {seto_uuid_128,74302962988791779,0},
                            member_requested}...}
    7> smk_client:subscribe(smk_my_callbacks, #seto_uuid_128{low=122001}).
    {ok, 4}

As you can see from the above example the actual calls return ok and their outgoing sequence number, this means the message was sent to Smarkets, the response is asyncronously sent back from Smarkets and is in this example simply printed out by lager (logging). By being asyncronous it is possible to make more than one order before receiving the confirmation from the first.

