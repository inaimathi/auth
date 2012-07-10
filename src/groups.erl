-module(groups).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("tables.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([list/0, list/1, add/1, add/2, rename/2, add_to/2, remove_from/2]).
-export([create/0]).

list() -> 
    gen_server:call(?MODULE, {list, []}).
list(Parent) when is_tuple(Parent) -> 
    gen_server:call(?MODULE, {list, [Parent]});
list(Parents) -> 
    gen_server:call(?MODULE, {list, Parents}).

add(GroupName) ->
    gen_server:call(?MODULE, {new_group, GroupName}).
add(GroupName, ParentGroupId) ->
    gen_server:call(?MODULE, {insert_subgroup, GroupName, ParentGroupId}).

rename(GroupId, NewName) -> gen_server:call(?MODULE, {change, name, GroupId, NewName}).

add_to(ParentGroupId, {group, ChildGroupId}) -> 
    gen_server:call(?MODULE, {add_group, find(ParentGroupId), find(ChildGroupId)});
add_to(GroupId, {user, UserName}) ->
    gen_server:call(?MODULE, {add_user, find(GroupId), users:find(UserName)}).
remove_from(ParentGroupId, {group, ChildGroupId}) -> 
    gen_server:call(?MODULE, {remove_group, find(ParentGroupId), find(ChildGroupId)});
remove_from(GroupId, {user, UserName}) ->
    gen_server:call(?MODULE, {remove_user, find(GroupId), users:find(UserName)}).

handle_call({list, Parents}, _From, State) ->
    Res = db:do(qlc:q([X || X <- mnesia:table(group), X#group.parent_groups =:= Parents])),
    {reply, Res, State};
handle_call({new_group, GroupName}, _From, State) -> 
    Id = now(),
    db:atomic_insert(#group{id=Id, name=GroupName}),
    {reply, Id, State};
handle_call({insert_subgroup, GroupName, ParentGroupId}, _From, State) ->
    ParentGroup = find(ParentGroupId), 
    Id = now(),
    Record = #group{id=Id, name=GroupName, parent_groups=[ParentGroupId]},
    F = fun() -> mnesia:write(Record), 
		 mnesia:write(db:push_to(#group.groups, ParentGroup, Record#group.id))
	end,
    ok = db:transaction(F),
    {reply, Id, State};
handle_call({add_group, Parent, Child}, _From, State) ->
    Val = db:relate(Parent, #group.id, #group.groups, Child, #group.id, #group.parent_groups),
    {reply, Val, State};
handle_call({add_user, Group, User}, _From, State) ->
    Val = db:relate(Group, #group.id, #group.users, User, #user.id, #user.groups),
    {reply, Val, State};
handle_call({remove_group, Parent, Child}, _From, State) ->
    Val = db:decouple(Parent, #group.id, #group.groups, Child, #group.id, #group.parent_groups),
    {reply, Val, State};
handle_call({remove_user, Group, User}, _From, State) ->
    Val = db:decouple(Group, #group.id, #group.users, User, #user.id, #user.groups),
    {reply, Val, State};
handle_call({change, name, GroupId, NewValue}, _From, State) ->
    Rec = find(GroupId),
    NewRec = Rec#group{name=NewValue},
    {reply, db:atomic_insert(NewRec), State}.

%%%%%%%%%%%%%%%%%%%% database related
find(GroupId) ->
    [Rec] = db:do(qlc:q([X || X <- mnesia:table(group), X#group.id =:= GroupId])),
    Rec.

create() -> 
    mnesia:create_table(group, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, group)}]).

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
