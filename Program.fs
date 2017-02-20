open System

#nowarn "25"

type DoorState = 
    | Goat
    | Car
    | Revealed

let monty_hall_simulator d o c (r: Random) = 
    // Initialize doors.
    let doors = Array.create d Goat
    for i in 1..c do
        let mutable door = r.Next d
        while doors.[door] <> Goat do
            door <- r.Next d
        doors.[door] <- Car

    // Reveal some doors that contain goats.
    for i in 1..o do
        let mutable door = r.Next(1,d)
        while doors.[door] <> Goat do
            door <- r.Next(1,d)
        doors.[door] <- Revealed

    // Look for the first door that has not been revealed
    match Array.find (fun elem -> elem <> Revealed) doors.[1..] with
    | Goat -> 0
    | Car -> 1
    | Revealed -> failwith "logic error: unreachable code!"

let iterated_monty_hall n d o c =
    let random = new Random()
    let monty_hall = monty_hall_simulator d o c
    Seq.init n (fun _ -> monty_hall random) |> Seq.sum

(* 
        With 3 doors, 1 car, 1 reveal
        intuition tells us 50% chance of winning
        thus, we beat intuition if we beat 50%

        How do we come up with the intuitive solution?
        3 doors - 1 reveal = 2 doors remaining
        1 door out of 2 contains a car
*)



let evaluate n d o c s =
    let winratio = (decimal s) / (decimal n)
    let intuition = (decimal c) / ((decimal d) - (decimal o))

    printfn "Number of trials: %d" n
    printfn "Number of successes: %d" s
    printfn "Ratio of successes to wins: %M" winratio
    printfn "Flawed intuitive expected success ratio: %M" intuition

    if winratio > intuition then
        printfn "Conclusion: Switch strategy is a good idea!"
    else
        printfn "Conclusion: Switch strategy not necessarily useful."

let usage() = 
    printfn "Usage: MontyHall trials doors reveals cars"
    printfn "All arguments are mandatory and must be positive integers"
    printfn "  trials                Number of attempts to win"
    printfn "  doors                 Number of total doors"
    printfn "  reveals               Number of doors opened"
    printfn "  cars                  Number of cars behind doors"
    exit 0

[<EntryPoint>]
let main argv =
    if Array.length(argv) < 4 then usage()

    let nonposerr = "Command line arguments must be positive integers."
    let revealerr = "Monty Hall cannot open more doors than exist!"
    let carserror = "Monty Hall cannot reveal any doors with cars!"

    let parse_positive(str: string) =
        match Convert.ToInt32 str with 
        | m when m < 1 -> failwith nonposerr
        | m -> m

    try
        let [|n; d; o; c|] = Array.map parse_positive argv.[..3]

        if d < o     then failwith revealerr
        if d < (o+c) then failwith carserror
        
        iterated_monty_hall n d o c |> evaluate n d o c
        0
    with
        | :? FormatException ->
            printfn "%s" nonposerr
            -1
        | ex ->
            printfn "%s" ex.Message
            -1