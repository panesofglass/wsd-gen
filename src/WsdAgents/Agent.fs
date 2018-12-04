namespace WsdAgents

type State =
    | State of name:string

type Message =
    | Message of name:string * data:string

type Transition =
    { FromState : State
      Message : Message
      ToState : State }

type internal AgentMessage =
    | GetState of AsyncReplyChannel<State * Transition list>
    | Subscribe of transition:Transition * handler:(Message -> unit)
    | Transition of message:Message

module Agent =

    let allowedTransitions state (transitions:Transition list) =
        transitions
        |> List.filter (fun t -> t.FromState = state)
    
    let tryFindAllowedTransition state message (transitions:Transition list) =
        transitions
        |> List.tryFind (fun t ->
            let (Message(expected, _)) = t.Message
            let (Message(actual, _)) = message
            t.FromState = state && expected = actual)

type Agent (identifier:string, initState:State, transitions:Transition list) =

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop state (handlers:Map<Transition, Message -> unit>) = async {
                match! inbox.Receive() with
                | GetState(channel) ->
                    let allowedTransitions = Agent.allowedTransitions state transitions
                    channel.Reply(state, allowedTransitions)
                    return! loop state handlers
                | Subscribe(transition, handler) ->
                    match List.tryFind ((=) transition) transitions with
                    | Some _ ->
                        return! loop state (Map.add transition handler handlers)
                    | None ->
                        eprintfn "Coud not add handler for missing transition: %A" transition
                        return! loop state handlers
                | Transition message ->
                    match Agent.tryFindAllowedTransition state message transitions with
                    | Some transition ->
                        match Map.tryFind transition handlers with
                        | Some handler ->
                            handler message
                        | None ->
                            printfn "No handler registered for transition: %A" transition
                        // Set the new state and return.
                        return! loop transition.ToState handlers
                    | None ->
                        eprintfn "Could not find transition from %A on %A" state message
                        return! loop state handlers
            }
            loop initState Map.empty
        )

    member __.Identifier = identifier

    member __.Get() =
        agent.PostAndReply(GetState)

    member __.GetAsync() =
        agent.PostAndAsyncReply(GetState)

    member __.Post(message) =
        agent.Post(Transition message)

    member __.Subscribe(transition, handler) =
        agent.Post(Subscribe(transition, handler))
