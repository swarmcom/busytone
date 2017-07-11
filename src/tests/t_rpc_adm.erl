-module(t_rpc_adm).
-export([main/0]).

main() ->
	Profile = admin_user:new_profile(),
	Agent = admin_user:new_agent(#{ profile => Profile }),
	{match, _, #{ <<"result">> := Skills }} = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_skills">>, [Agent]),
	{match, _, #{ <<"result">> := <<"ok">> }} = agent:rpc_call(Agent, <<"ouc_rpc_adm.set_agent_skills">>, [Agent, Skills#{ english => true }]),
	{match, _, #{ <<"result">> := #{ <<"english">> := true } }} = agent:rpc_call(Agent, <<"ouc_rpc_adm.agent_skills">>, [Agent]).