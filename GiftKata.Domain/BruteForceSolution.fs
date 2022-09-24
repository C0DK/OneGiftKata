module GiftKata.Domain.BruteForceSolution

    
    
let completePotentialSolution (party : Christmas) (solution : Solution) =
    if solution.IsValid then Some solution else None
    
let possibleReceivers (party : Christmas) (person : Person) =
    party.GiftAbilities |> Seq.filter (fun giftAbility -> giftAbility.From = person)

let bruteForce (party : Christmas) =
    
    party.Members
        |> Seq.map (fun person -> { Members = set [person]; Gifts= Set.empty  })
        |> Seq.map (completePotentialSolution party)
        |> Seq.collect (fun option ->
                        match option with
                        | Some value -> seq [value]
                        | None -> Seq.empty)
        |> Seq.tryHead
    