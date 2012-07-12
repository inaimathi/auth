-module(m2crypto).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([encrypt/2, verify/3, split_key/1, decrypt/2, sign/2]).

encrypt({E, N}, Message) -> gen_server:call(?MODULE, {encrypt, E, N, Message}).
verify({E, N}, Message, Signature) -> gen_server:call(?MODULE, {verify, E, N, Message, Signature}).
split_key(Filename) -> gen_server:call(?MODULE, {split_key, Filename}).

decrypt(PkPem, Message) -> gen_server:call(?MODULE, {decrypt, PkPem, Message}).
sign(PkPem, Message) -> gen_server:call(?MODULE, {sign, PkPem, Message}).
    
handle_call({'EXIT', _Port, Reason}, _From, _State) ->
    exit({port_terminated, Reason});
handle_call(Message, _From, Port) ->
    port_command(Port, term_to_binary(Message)),
    receive
	{State, {data, Data}} -> 
	    {reply, binary_to_term(Data), State}
    after 3000 -> 
	    exit(timeout)
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
get_pyc(Name) when is_atom(Name) ->
    ModDir = filename:dirname(code:which(?MODULE)),
    filename:join([ModDir, "..", "priv", atom_to_list(Name) ++ ".pyc"]).    
init([]) -> 
    Pyc = get_pyc(?MODULE),
    {ok, open_port({spawn, "python -u " ++ Pyc}, [{packet, 4}, binary, use_stdio])}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
