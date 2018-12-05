module Tests

open System
open Expecto
open WsdAgents

type State =
    | State of name:string

type Message =
    | Message of name:string * data:string

let createAgent initState =
    let transitions = [
        // home->+WIP: startOnboarding(identifier)
        { FromState = State "home"
          ToState = State "WIP"
          Message = Message("startOnboarding", "identifier") }
        // WIP->+customerData: collectCustomerData(identifier,name,email)
        { FromState = State "WIP"
          ToState = State "customerData"
          Message = Message("collectCustomerData", "identifier,name,email") }
        // customerData-->-WIP: saveToWIP(identifier,name,email)
        { FromState = State "customerData"
          ToState = State "WIP"
          Message = Message("saveToWIP", "identifier,name,email") }
        // WIP->+accountData: collectAccountData(identifier,region,discount)
        { FromState = State "WIP"
          ToState = State "accountData"
          Message = Message("collectAccountData", "identifier,region,discount") }
        // accountData-->-WIP:saveToWIP(identifier,region,discount)
        { FromState = State "accountData"
          ToState = State "WIP"
          Message = Message("saveToWIP", "identifier,region,discount") }
        // WIP-->+finalizeWIP:completeOnboarding(identifier)
        { FromState = State "WIP"
          ToState = State "finalizeWIP"
          Message = Message("completeOnboarding", "identifier") }
        // finalizeWIP->-home:goHome
        { FromState = State "finalizeWIP"
          ToState = State "home"
          Message = Message("goHome", "") }
        // WIP-->+cancelWIP:abandonOnboarding(identifier)
        { FromState = State "WIP"
          ToState = State "cancelWIP"
          Message = Message("abandonOnboarding", "identifier") }
        // cancelWIP->-home:goHome
        { FromState = State "cancelWIP"
          ToState = State "home"
          Message = Message("goHome", "") }
    ]
    Agent(Uri "urn:agent:1", initState, transitions, function (Message(expected,_)), (Message(actual,_)) -> expected = actual)

[<Tests>]
let tests =
    testSequenced <| testList "agents" [
        test "agent has correct identifier" {
            let expected = Uri "urn:agent:1"
            let agent = createAgent (State "home")
            let actual = agent.Identifier
            Expect.equal actual expected "Identifier should be an empty string."
        }

        test "agent starts in 'home' state" {
            let expected = State "home"
            let agent = createAgent (State "home")
            let actual, _ = agent.Get()
            Expect.equal actual expected "Should have been able to transition only to WIP."
        }

        test "agent can transition to 'WIP' from 'home'" {
            let expected = [
                { FromState = State "home"
                  ToState = State "WIP"
                  Message = Message("startOnboarding", "identifier") }
            ]
            let agent = createAgent (State "home")
            let _, actual = agent.Get()
            Expect.equal actual expected "Should have been able to transition only to WIP."
        }

        test "agent transitions to 'WIP' after receiving a message of 'startOnboarding'" {
            let expected =
                State "WIP", [
                    { FromState = State "WIP"
                      ToState = State "customerData"
                      Message = Message("collectCustomerData", "identifier,name,email") }
                    { FromState = State "WIP"
                      ToState = State "accountData"
                      Message = Message("collectAccountData", "identifier,region,discount") }
                    { FromState = State "WIP"
                      ToState = State "finalizeWIP"
                      Message = Message("completeOnboarding", "identifier") }
                    { FromState = State "WIP"
                      ToState = State "cancelWIP"
                      Message = Message("abandonOnboarding", "identifier") }
                ]
            let agent = createAgent (State "home")
            agent.Post(Message("startOnboarding", ""))
            let actual = agent.Get()
            Expect.equal actual expected "Should transition to WIP state with 4 transitions."
        }

        test "agent transitions to 'finalizeWIP' after receiving a message of 'completeOnboarding'" {
            let expected =
                State "finalizeWIP", [
                    { FromState = State "finalizeWIP"
                      ToState = State "home"
                      Message = Message("goHome", "") }
                ]
            let agent = createAgent (State "WIP")
            agent.Post(Message("completeOnboarding", ""))
            let actual = agent.Get()
            Expect.equal actual expected "Should transition to finalizeWIP state with 1 transition to home."
        }

        test "agent transitions to 'home' from 'finalizeWIP' after receiving a message of 'goHome'" {
            let expected =
                State "home", [
                    { FromState = State "home"
                      ToState = State "WIP"
                      Message = Message("startOnboarding", "identifier") }
                ]
            let agent = createAgent (State "finalizeWIP")
            agent.Post(Message("goHome", ""))
            let actual = agent.Get()
            Expect.equal actual expected "Should transition to home state with 1 transition to WIP."
        }

        test "agent transitions to 'cancelWIP' after receiving a message of 'abandonOnboarding'" {
            let expected =
                State "cancelWIP", [
                    { FromState = State "cancelWIP"
                      ToState = State "home"
                      Message = Message("goHome", "") }
                ]
            let agent = createAgent (State "WIP")
            agent.Post(Message("abandonOnboarding", ""))
            let actual = agent.Get()
            Expect.equal actual expected "Should transition to cancelWIP state with 1 transition to home."
        }

        test "agent transitions to 'home' from 'cancelWIP' after receiving a message of 'goHome'" {
            let expected =
                State "home", [
                    { FromState = State "home"
                      ToState = State "WIP"
                      Message = Message("startOnboarding", "identifier") }
                ]
            let agent = createAgent (State "cancelWIP")
            agent.Post(Message("goHome", ""))
            let actual = agent.Get()
            Expect.equal actual expected "Should transition to home state with 1 transition to WIP."
        }
    ]

