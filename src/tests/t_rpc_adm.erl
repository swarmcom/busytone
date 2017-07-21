-module(t_rpc_adm).
-export([main/0]).

main() ->
	Profile = admin:new_profile(),
	Login = admin:new_agent(#{ profile => Profile }),
	#{ <<"login">> := <<"test_agent_1_login">> } = admin:get_agent(Login),
	#{ <<"name">> := <<"test_profile_1_name">> } = admin:get_profile(Profile).
