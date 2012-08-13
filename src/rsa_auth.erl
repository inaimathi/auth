-module(rsa_auth).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("tables.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([gen_secret/2, verify/3, new_key/2, change_key/2]).
-export([create/0, get_key/1, list/0]).

-record(pubkey, {user_id, pubkey}).
-record(secret, {timestamp, user_id, user_meta, plaintext}).

%%%%%%%%%% external API
list() -> %% lists all user_id/pubkey pairs
    gen_server:call(?MODULE, list).
get_key(UserId) -> %% returns a given users' key, or 'false' if that user has no key
    gen_server:call(?MODULE, {find_key, UserId}).
new_key(UserId, Pubkey) ->  %% adds the given key to the given user
    gen_server:call(?MODULE, {new_key, UserId, Pubkey}).
change_key(UserId, Pubkey) -> %% changes the key for an existing user
    gen_server:call(?MODULE, {change_key, UserId, Pubkey}).
gen_secret(UserId, Meta) -> %% generates a new secret for the specified user with specified metadata (usually IP and user agent)
    gen_server:call(?MODULE, {gen_secret, UserId, Meta}).
verify(UserId, Meta, Sig) -> %% verifies a given Meta/Signature combination for a given user
    gen_server:call(?MODULE, {verify, UserId, Meta, Sig}).
%%%%%%%%%%%%%%%%%%%%%%%

handle_call(list, _From, Keys) ->
    {reply, db:list(pubkey, [#pubkey.user_id, #pubkey.pubkey]), Keys};
handle_call({find_key, UserId}, _From, Keys) ->
    #pubkey{pubkey=Pubkey}=find(UserId),
    {reply, Pubkey, Keys}; 
handle_call({gen_secret, User, Meta}, _From, [Pk, Pub]) -> 
    #user{id=UserId} = users:find(User),
    #pubkey{pubkey=Pubkey}=find(UserId),
    P = common:binary_to_hex(crypto:sha(crypto:rand_bytes(32))),
    Ciphertext = m2crypto:encrypt(Pubkey, P),
    Secret = #secret{timestamp=now(), user_id=UserId, user_meta=Meta, plaintext=P},
    db:transaction(fun() -> mnesia:write(Secret) end),
    Sig = m2crypto:sign(Pk, Ciphertext),
    {reply, {Ciphertext, Sig}, [Pk, Pub]};
handle_call({verify, UserId, Meta, Sig}, _From, Keys) ->
    #pubkey{pubkey=Pubkey} = find(UserId),
    Secrets = get_secrets(UserId, Meta),
    Res = lists:any(
	    fun({T, S}) -> verify_key({T, S}, Pubkey, Sig) end, 
	    Secrets),
    {reply, Res, Keys};
handle_call({change_key, UserId, Pubkey}, _From, Keys) -> 
    Res = case find(UserId) of
	      false -> false;
	      {_E, _N} -> push_key(UserId, Pubkey)
	  end,
    {reply, Res, Keys};
handle_call({new_key, UserId, Pubkey}, _From, Keys) -> 
    Res = case find(UserId) of
	      false -> push_key(UserId, Pubkey);
	      {_E, _N} -> already_exists
	  end,
    {reply, Res, Keys}.

%%% rsa_auth-specific utility
push_key(UserId, Pubkey) ->
    Fname = common:make_tempname(),
    file:write_file(Fname, Pubkey),
    K = m2crypto:split_key(Fname),
    Rec = #pubkey{user_id=UserId, pubkey=K},
    db:transaction(fun() -> mnesia:write(Rec) end),
    file:delete(Fname),
    K.

verify_key({T, S}, Pubkey, Sig) ->
    case old_secret_p(T) of
	true -> revoke_secret(T),
		false;
	_ -> case m2crypto:verify(Pubkey, S, Sig) of
		 true -> revoke_secret(T),
			 true;
		 _ -> false
	     end
    end.

revoke_secret(T) ->
    db:transaction(fun() -> mnesia:delete({secret, T}) end).

old_secret_p(T) -> 
    %% it's old if the timestamp is older than 5 minutes
    300 < (common:now_to_seconds(now()) - common:now_to_seconds(T)).

%%%%%%%%%%%%%%%%%%%% DB-related utility
find(UserId) -> db:find(pubkey, #pubkey.user_id, UserId).
get_secrets(UserId, Meta) ->
    db:do(qlc:q([{X#secret.timestamp, X#secret.plaintext} 
		 || X <- mnesia:table(secret), 
		    X#secret.user_id =:= UserId, 
		    X#secret.user_meta =:= Meta])).

create() ->
    mnesia:create_table(pubkey, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, pubkey)}]),
    mnesia:create_table(secret, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, secret)}]).

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> 
    DirName = case code:priv_dir(?MODULE) of
		  {error, bad_name} ->
		      %% this is here for testing purposes
		      filename:join(
			[filename:dirname(
			   code:which(?MODULE)),"..","priv"]);
		  Dir -> Dir
	      end,
    PkFile = filename:join(DirName, "server_auth.key"),
    PubFile = filename:join(DirName, "server_auth.pub"),
    {ok, Pk} = file:read_file(PkFile),
    Pub = m2crypto:split_key(PubFile),
    {ok, [Pk, Pub]}.
handle_cast(_Msg, Keys) -> {noreply, Keys}.
handle_info(_Info, Keys) -> {noreply, Keys}.
terminate(_Reason, _Keys) -> ok.
code_change(_OldVsn, Keys, _Extra) -> {ok, Keys}.
