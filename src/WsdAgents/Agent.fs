namespace WsdAgents

open System

type Transition<'State, 'Message> =
    { FromState : 'State
      Message : 'Message
      ToState : 'State }

type internal AgentMessage<'State, 'Message> =
    | GetState of AsyncReplyChannel<'State * Transition<'State, 'Message> list>
    | Subscribe of transition:Transition<'State, 'Message> * handler:('Message -> unit)
    | Transition of message:'Message

module Agent =

    let allowedTransitions state (transitions:Transition<'State, 'Message> list) =
        transitions
        |> List.filter (fun t -> t.FromState = state)
    
    let tryFindAllowedTransition comparer state message (transitions:Transition<'State, 'Message> list) =
        transitions
        |> List.tryFind (fun t -> t.FromState = state && comparer(t.Message, message))

type Agent<'State, 'Message when 'State : comparison and 'Message : comparison> (identifier:Uri, initState:'State, transitions:Transition<'State, 'Message> list, comparer) =

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop state (handlers:Map<Transition<'State, 'Message>, 'Message -> unit>) = async {
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
                    match Agent.tryFindAllowedTransition comparer state message transitions with
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
