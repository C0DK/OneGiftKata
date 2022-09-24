module Tests.Solution
open Expecto
open GiftKata.Domain

let alice = { Name = "Alice" }
let bob = { Name = "Bob" }

module Expect =
    let isValid solution =
        Expect.isTrue (Solution.isValid solution) "Solution was not valid!"
        
    let isInvalid solution =
        Expect.isFalse (Solution.isValid solution) "Solution was valid - expected invalid!"

[<Tests>]
let solution =
  testList "Solution" [
      testList "IsValid" [
        testCase "No gifts is valid for no persons" <| fun _ ->
            {
                Members = Set.empty
                Gifts = Set.empty
            }
            |> Expect.isValid

        testCase "No gifts is NOT valid for 2 persons" <| fun _ ->
            {
                Members = Set [ alice; bob ]
                Gifts = Set.empty
            }
            |> Expect.isInvalid

        testCase "One gift is invalid for 2 persons" <| fun _ ->
            {
                Members = Set [ alice; bob ]
                Gifts = Set [ { From = alice; To = bob } ]
            }
            |> Expect.isInvalid
            
        testCase "two gifts from same person is invalid for 2 persons" <| fun _ ->
            {
                Members = Set [ alice; bob ]
                Gifts =
                    Set [ { From = alice; To = bob }
                          { From = alice; To = bob } ]
            }
            |> Expect.isInvalid

        testCase "two gifts to each other is valid for 2 persons" <| fun _ ->
            {
                Members = Set [ alice; bob ]
                Gifts =
                    Set [ { From = alice; To = bob }
                          { From = bob; To = alice } ]
            }
            |> Expect.isValid
      ]
  ]
  