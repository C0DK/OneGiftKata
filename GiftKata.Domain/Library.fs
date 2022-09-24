namespace GiftKata.Domain

type Person = { Name: string }

type Gift = { From: Person; To: Person }

module Seq =
    let HasExactlyOne predicate items =
        match items |> Seq.filter predicate |> Seq.tryExactlyOne with
        | Some _ -> true
        | None -> false

type Christmas =
    {
        Members: Person Set
        GiftAbilities: Gift Set
    }

type Solution =
    {
        Members: Person Set
        Gifts: Gift Set
    }

    member this.IsValid =
        this.EveryoneGetsExactlyOneGift && this.EveryoneGivesExactlyOneGift

    member private this.EveryoneGetsExactlyOneGift =
        this.Members
        |> Seq.forall (fun person ->
            this.Gifts
            |> Seq.HasExactlyOne(fun gift -> gift.To = person))

    member private this.EveryoneGivesExactlyOneGift =
        this.Members
        |> Seq.forall (fun person ->
            this.Gifts
            |> Seq.HasExactlyOne(fun gift -> gift.From = person))

module Solution =
    let isValid (solution : Solution) = solution.IsValid