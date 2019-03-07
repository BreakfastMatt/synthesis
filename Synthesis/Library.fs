module Synthesis

let abelar a =
    match ((12 < a  && a < 3097) && a % 12=0) with | false -> false | _-> true
        
let area b h =
    match (b>=0.0 && h>=0.0) with 
     |false -> failwith "Cannot have a negative base or height"
     |_ -> (b * h *0.5)

let zollo x =
    match x>=0 with 
     |true -> (x * 2)
     |_ -> (x * -1)
     
let min a b =
    match a < b with
     |true -> a
     |_ -> b

let max _ _ =
    failwith "Not implemented"

let ofTime _ _ _ =
    failwith "Not implemented"

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"