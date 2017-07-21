-module(t_agent_effective_skills).
-export([main/0]).

main() ->
	Profile = admin:new_profile(),
	Login = admin:new_agent(#{ profile => Profile }),
	false = maps:is_key(<<"english">>, admin:rpc_call(<<"ouc_rpc_adm.agent_effective_skills">>, [Login])),
	admin:update_agent(Login, #{ skills => #{ english => true }}),
	M = #{ <<"english">> := true } = admin:rpc_call(<<"ouc_rpc_adm.agent_effective_skills">>, [Login]),
	false = maps:is_key(<<"german">>, M),
	admin:update_profile(Profile, #{ skills => #{ german => true }}),
	#{ <<"english">> := true, <<"german">> := true } = admin:rpc_call(<<"ouc_rpc_adm.agent_effective_skills">>, [Login]).
