-module(t_rpc_adm).
-export([main/0]).

main() ->
	Profile = admin_user:new_profile(),
	Agent = admin_user:new_agent(#{ profile => Profile }),
	Skills = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_skills">>, [Agent]),
	false = maps:is_key(<<"english">>, agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_skills">>, [Agent])),
	<<"ok">> = agent:rpc_call(Agent, <<"ouc_rpc_adm.set_agent_skills">>, [Agent, Skills#{ english => true }]),
	#{ <<"english">> := true } = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_skills">>, [Agent]),
	#{ <<"english">> := true } = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_effective_skills">>, [Agent]),
	<<"ok">> = agent:rpc_call(Agent, <<"ouc_rpc_adm.update_profile">>, [Profile, #{ skills => #{ german => true }}]),
	#{ <<"english">> := true, <<"german">> := true } = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_effective_skills">>, [Agent]).
