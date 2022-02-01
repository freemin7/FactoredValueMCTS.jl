struct LastActionWrapper{S,A} <: JointMDP{S,A}
  SubProblem::JointMDP{S,A}
  LastAction::Vector{Int64}
end

function last_actionindex(LS)
  LS.LastAction
end

function LastActionWrapper(SubProblem)
  LastActionWrapper(SubProblem, #==Vector{Int64}(undef, n_agents(SubProblem)==# ones(Int64,MultiAgentPOMDPs.n_agents(SubProblem)))
end

function MultiAgentPOMDPs.MultiAgentPOMDPs.n_agents(LS::LastActionWrapper) 
  MultiAgentPOMDPs.n_agents(LS.SubProblem)
end

function MultiAgentPOMDPs.agent_actions(LS::LastActionWrapper, idx::Int64, s::S) where S
  MultiAgentPOMDPs.agent_actions(LS.SubProblem, idx, s)
end

function MultiAgentPOMDPs.agent_actions(LS::LastActionWrapper, idx::Int64) 
  MultiAgentPOMDPs.agent_actions(LS.SubProblem, idx)
end

function MultiAgentPOMDPs.agent_states(LS::LastActionWrapper, idx::Int64) 
  MultiAgentPOMDPs.agent_states(LS.SubProblem, idx)
end

function MultiAgentPOMDPs.agent_actionindex(LS::LastActionWrapper, idx::Int64, a::A) where A
  MultiAgentPOMDPs.agent_actionindex(LS.SubProblem, idx, a)
end


function MultiAgentPOMDPs.coordination_graph(LS::LastActionWrapper)
  MultiAgentPOMDPs.coordination_graph(LS.SubProblem)
end

function MultiAgentPOMDPs.coordination_graph(LS::LastActionWrapper, s::S) where S
  MultiAgentPOMDPs.coordination_graph(LS.SubProblem, s)
end

function POMDPs.discount(LS::LastActionWrapper)
  POMDPs.discount(LS.SubProblem)
end

function POMDPs.gen(LS::LastActionWrapper, state, action, rng)
  for i in 1:(n_agents(LS.SubProblem))
    LS.LastAction[i] = agent_actionindex(LS.SubProblem, i, action[i])
  end
  
  POMDPs.gen(LS.SubProblem, state, action, rng)
end
#== THIS DOESN'T WORK. EVEN WHEN UNCOMMENTED AS UAV example doesn't define a transition directly.
function POMDPs.transition(LS::LastActionWrapper, state, action)
  transition(LS.SubProblem, state, action)
  for i in 1:(n_agents(SubProblem))
    LS.LastAction[i] = agent_actionindex(LS.SubProblem, i, action[i])
  end
end 
==#
function POMDPs.reward(LS::LastActionWrapper, s, a)
  POMDPs.reward(LS.SubProblem, s, a)
end

function POMDPs.reward(LS::LastActionWrapper, s, a, sp)
  POMDPs.reward(LS.SubProblem, s, a, sp)
end

function POMDPs.isterminal(LS::LastActionWrapper, state) 
  POMDPs.isterminal(LS.SubProblem, state)
end

function POMDPs.initialstate(LS::LastActionWrapper)
  POMDPs.initialstate(LS.SubProblem)
end

function POMDPs.stateindex(LS::LastActionWrapper, s)
   POMDPs.stateindex(LS.SubProblem, s)
end

function POMDPs.actionindex(LS::LastActionWrapper, a)
   POMDPs.actionindex(LS.SubProblem, a)
end
#==
function convert_s(::Type{V}, s, LS::LastActionWrapper) where V<:AbstractArray
  
end

function convert_s(::Type{S}, vec::V, LS::LastActionWrapper) where {S,V<:AbstractArray}

end

function convert_a(::Type{V}, a, LS::LastActionWrapper) where V<:AbstractArray

end

function convert_a(::Type{A}, vec::V, LS::LastActionWrapper) where {A,V<:AbstractArray}

end
==#
