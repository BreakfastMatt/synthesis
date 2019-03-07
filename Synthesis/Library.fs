module Synthesis

let abelar a = //return true if value is greater than 12, but less than 3097 and is a multiple of 12, otherwise return false.
    match ((12 < a  && a < 3097) && a % 12=0) with | false -> false | _-> true
        
let area b h = //throw exception if base or height is negative, otherwise calculate area of triangle.
    match (b>=0.0 && h>=0.0) with 
     |false -> failwith "Cannot have a negative base or height"
     |_ -> (b * h *0.5)

let zollo x = //double x if it is positive, or make it positive if it is negative.
    match x>=0 with 
     |true -> (x * 2)
     |_ -> (x * -1)
     
let min a b = //return the smaller of the two values
    match a < b with
     |true -> a
     |_ -> b

let max a b = //return the the larger of the two values
    match a > b with
     |true -> a
     |_ -> b 

let ofTime h m s = //converts hours, minutes & seconds to seconds. 
    (((h * 60) + m)*60) + s  

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