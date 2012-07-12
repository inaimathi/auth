-module(test).
-export([run/1]).

run([Username, GroupName]) ->
    Password = "fiddlesticks",
    test_users(Username, Password),
    test_groups(GroupName, Username),
    test_rsa_auth(Username),
    io:format("All Tests Passed\n\n").

test_users(Username, Password) ->
    io:format("testing `users`\n"),
    Users = users:list(), true = is_list(Users),
    {UserId, Username, []} = users:register(Username, Password),
    already_exists = users:register(Username, Password),
    {UserId, Username, Groups} = users:get(UserId),
    {UserId, Username, Groups} = users:get(Username),
    {UserId, Username, Groups} = users:auth(Username, Password),
    NewPass = Password ++ "nope",
    io:format("*********** expected error incoming ***********\n"),
    false = users:auth(Username, NewPass),
    io:format("*********** expected error resolved ***********\n"),
    ok = users:change_password(Username, NewPass),
    {UserId, Username, Groups} = users:auth(Username, NewPass),
    io:format("`users` Passed\n\n").

test_groups(GroupName, Username) ->
    io:format("testing `groups`\n"),
    io:format("   setting up test state\n"),
    NewName = "New And Improved " ++ GroupName, 
    Sub1 = "IT", Sub2 = "Finance",
    {UserId, Username, _} = users:get(Username),
    
    io:format("   testing basics\n"),
    Groups = groups:list(), true = is_list(Groups),
    GroupId = groups:add(GroupName),
    [] = groups:list(GroupId),
    ok = groups:rename(GroupId, NewName),

    io:format("   testing subgroups\n"),
    %%% subgroups (direct add, and association)
    SubgroupId = groups:add(Sub1),
    Subgroup2Id = groups:add(Sub2, GroupId),
    [{group,Subgroup2Id,Sub2,[GroupId],[],[]}] = groups:list(GroupId),
    ok = groups:remove_from(GroupId, {group, Subgroup2Id}),
    [] = groups:list(GroupId),
    ok = groups:add_to(GroupId, {group, Subgroup2Id}),
    [{group,Subgroup2Id,Sub2,[GroupId],[],[]}] = groups:list(GroupId),
    [{group,Subgroup2Id,Sub2,[GroupId],[],[]}] = groups:list([GroupId]),
    ok = groups:add_to(GroupId, {group, SubgroupId}),
    [{group,_,_,[_],[],[]}, {group,_,_,[_],[],[]}] = groups:list(GroupId),
    [{group,_,_,[_],[],[]}, {group,_,_,[_],[],[]}] = groups:list([GroupId]),
    MoreGroups = groups:list(), true = is_list(MoreGroups),

    io:format("   testing users\n"),
    %%% group members (adding/removing by user name then by user id, both of which are unique and therefore valid lookups)
    ok = groups:add_to(GroupId, {user, Username}),
    {UserId, UserName, [GroupId]} = users:get(Username),
    {group,GroupId,NewName,[],[UserId],_} = groups:get(GroupId),
    ok = groups:remove_from(GroupId, {user, Username}),
    {UserId, UserName, []} = users:get(Username),
    {group,GroupId,NewName,[],[],_} = groups:get(GroupId),

    ok = groups:add_to(GroupId, {user, UserId}),
    {UserId, UserName, [GroupId]} = users:get(Username),
    {group,GroupId,NewName,[],[UserId],_} = groups:get(GroupId),
    ok = groups:remove_from(GroupId, {user, UserId}),
    {UserId, UserName, []} = users:get(Username),
    {group,GroupId,NewName,[],[],_} = groups:get(GroupId),
    io:format("`groups` Passed\n\n").

test_rsa_auth(Username) ->
    io:format("testing `rsa_auth`\n"),
    io:format("   setting up test state\n"),
    UserId = users:get(Username),
    {ok, PubKeyPem} = file:read_file("src/server_auth.pub"),
    {ok, PrivKeyPem} = file:read_file("src/server_auth.key"),
    IP = "127.0.0.1", AttackerIP = "192.168.1.1",
    
    io:format("   testing basics\n"),   
    Res = rsa_auth:list(), true = is_list(Res),
    false = rsa_auth:get_key(UserId),
    false = rsa_auth:change_key(UserId,PubKeyPem),

    io:format("   testing new_key and change_key\n"),   
    {Exp, N} = rsa_auth:new_key(UserId, PubKeyPem),
    {Exp, N} = rsa_auth:get_key(UserId),
    {Exp, N} = rsa_auth:change_key(UserId,PubKeyPem),
    already_exists = rsa_auth:new_key(UserId, PubKeyPem),

    io:format("   testing gen_secret and verify\n"),   
    {Secret, Sig} = rsa_auth:gen_secret(UserId, IP),
    true = m2crypto:verify({Exp, N}, Secret, Sig),
    Clear = m2crypto:decrypt(PrivKeyPem, Secret), true = is_list(Clear),
    ClientSig = m2crypto:sign(PrivKeyPem, Clear),
    false = rsa_auth:verify(UserId, AttackerIP, ClientSig), 
    true = rsa_auth:verify(UserId, IP, ClientSig),
    false = rsa_auth:verify(UserId, IP, ClientSig),
    io:format("`rsa_auth` passed\n").
