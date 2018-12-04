#load "Agent.fs"

open WsdAgents

(*
home->+WIP: startOnboarding(identifier)
WIP->+customerData: collectCustomerData(identifier,name,email)
customerData-->-WIP: saveToWIP(identifier,name,email)
WIP->+accountData: collectAccountData(identifier,region,discount)
accountData-->-WIP:saveToWIP(identifier,region,discount)
WIP-->+finalizeWIP:completeOnboarding(identifier)
finalizeWIP->-home:goHome
WIP-->+cancelWIP:abandonOnboarding(identifier)
cancelWIP->-home:goHome
*)

let createAgent () =
    let initState = State "home"
    let transitions = [
        { FromState = State "home"
          ToState = State "WIP"
          Message = Message("startOnboarding", "identifier") }
        { FromState = State "WIP"
          ToState = State "customerData"
          Message = Message("collectCustomerData", "identifier,name,email") }
        { FromState = State "customerData"
          ToState = State "WIP"
          Message = Message("saveToWIP", "identifier,name,email") }
        { FromState = State "WIP"
          ToState = State "accountData"
          Message = Message("collectAccountData", "identifier,region,discount") }
        { FromState = State "accountData"
          ToState = State "WIP"
          Message = Message("saveToWIP", "identifier,region,discount") }
        { FromState = State "WIP"
          ToState = State "finalizeWIP"
          Message = Message("completeOnboarding", "identifier") }
        { FromState = State "finalizeWIP"
          ToState = State "home"
          Message = Message("goHome", "") }
        { FromState = State "WIP"
          ToState = State "cancelWIP"
          Message = Message("abandonOnboarding", "identifier") }
        { FromState = State "cancelWIP"
          ToState = State "home"
          Message = Message("goHome", "") }
    ]
    Agent("", initState, transitions)

let agent = createAgent()

agent.Identifier

let home, homeTransitions = agent.Get()

agent.Post(Message("startOnboarding",""))

let wip, wipTransitions = agent.Get()

