-module(firebase_auth_srv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3]).
-export([code_change/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {secret :: string(),
                expiration :: integer(),
                auth_claim :: list(),
                current_jwt :: binary(),
                current_creation_ts :: integer()
}).

start_link() ->
    gen_server:start_link({local, firebase_auth_srv}, firebase_auth_srv, [], []).

init(_Args) ->
    CL = application:get_all_env(firebase_erlang),
    State = #state {
        secret = proplists:get_value(auth_secret, CL, ""),
        expiration = proplists:get_value(auth_expiration, CL, 86400),
        auth_claim = proplists:get_value(auth_claim, CL, []),
        current_jwt = undefined,
        current_creation_ts = 0
    },
    {ok, State, hibernate}.

handle_call(get_token, _From, State) ->
    case State#state.current_jwt of
        undefined ->
            TS = now_as_time_t(),
            Jwt = create_jwt(TS, State),
            NewState = State#state {current_jwt = Jwt,
                                    current_creation_ts = TS
                                   },
            {reply, Jwt, NewState};
        Jwt ->
            CurrentTS = now_as_time_t(),
            CreationTS = State#state.current_creation_ts,
            Halflife = State#state.expiration / 2,
            if
                (CurrentTS - CreationTS) > Halflife ->
                    Jwt = create_jwt(CurrentTS, State),
                    NewState = State#state {current_jwt = Jwt,
                                            current_creation_ts = CurrentTS
                                           },
                    {reply, Jwt, NewState};
                true -> % else ...
                    Jwt = State#state.current_jwt,
                    {reply, Jwt, State}
            end
    end.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

% Private:
now_as_time_t() ->
    {A, B, _} = os:timestamp(),
    (A * 1000000) + B.

create_jwt(TS, State) ->
    Secret = State#state.secret,
    Expiration = State#state.expiration,
    AuthClaim = State#state.auth_claim,
    Claims = {[
        {v, 0},
        {iat, TS},
        {d, {AuthClaim}},
        {debug, true}
    ]},
    ejwt:jwt(<<"HS256">>, Claims, Expiration, Secret).
