-module(users).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("tables.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([list/0, get/1, register/2, change_password/2, auth/2]).
-export([create/0, find/1]).

list() -> gen_server:call(?MODULE, list).
register(Username, Password) -> gen_server:call(?MODULE, {register, Username, Password}).
get(Identifier) -> gen_server:call(?MODULE, {get_user, Identifier}).
change_password(Username, NewPassword) -> 
    gen_server:call(?MODULE, {change_pass, Username, NewPassword}).
auth(Username, Password) -> 
    Pid = self(),
    Auth = fun() -> User = find(Username),
		    true = salt(User#user.salt, Password) =:= User#user.password,
		    Pid ! User
	   end,
    AuthProc = spawn(Auth),
    receive
	User -> exit(AuthProc, thank_you),
		{User#user.id, User#user.username, User#user.groups}
    after 2000 -> 
	    false
    end.

handle_call(list, _From, State) -> 
    {reply, db:do(qlc:q([{X#user.id, X#user.username, X#user.groups} || X <- mnesia:table(user)])), State};
handle_call({get_user, Identifier}, _From, State) ->
    Res = case exists_p(Identifier) of
	      false -> false;
	      User -> {User#user.id, User#user.username, User#user.groups}
	  end,
    {reply, Res, State};
handle_call({register, Username, Password}, _From, State) -> 
    Res = case exists_p(Username) of
	      false -> Salt = crypto:rand_bytes(32),
		       Id = now(),
		       User = #user{id=Id, username=Username, password=salt(Salt, Password), salt=Salt},
		       db:transaction(fun() -> mnesia:write(User) end),
		       {Id, Username, []};
	      _ -> already_exists
	  end,
    {reply, Res, State};
handle_call({change_pass, Username, NewPassword}, _From, State) -> 
    User = find(Username),
    Salt = crypto:rand_bytes(32),
    {reply, db:transaction(fun() -> mnesia:write(User#user{password=salt(Salt, NewPassword), salt=Salt}) end), State}.

%%%%%%%%%%%%%%%%%%%% database related
salt(Salt, String) -> 
    common:binary_to_hex(erlsha2:sha256([Salt, String])).

create() -> 
    mnesia:create_table(user, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, user)}]).

exists_p(Username) -> 
    try
	find(Username)
    catch
	error:_ -> false
    end.

find(Id) when is_tuple(Id) ->
    [Rec] = db:do(qlc:q([X || X <- mnesia:table(user), X#user.id =:= Id])),
    Rec;
find(Name) -> 
    [Rec] = db:do(qlc:q([X || X <- mnesia:table(user), X#user.username =:= Name])),
    Rec.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
