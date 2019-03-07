module Synthesis

let abelar a = //return true if value is greater than 12, but less than 3097 and is a multiple of 12, otherwise return false.  [target: 1 line]
    match ((12 < a  && a < 3097) && a % 12=0) with | false -> false | _-> true
        
let area b h = //throw exception if base or height is negative, otherwise calculate area of triangle.  [target: 3 lines]
    match (b>=0.0 && h>=0.0) with 
     |false -> failwith "Cannot have a negative base or height"
     |_ -> (b * h *0.5)

let zollo x = //double x if it is positive, or make it positive if it is negative.  [target: 3 lines]
    match x>=0 with 
     |true -> (x * 2)
     |_ -> (x * -1)
     
let min a b = //return the smaller of the two values  [target: 3 lines]
    match a < b with
     |true -> a
     |_ -> b

let max a b = //return the the larger of the two values  [target: 3 lines]
    match a > b with
     |true -> a
     |_ -> b 

let ofTime h m s = //converts hours, minutes & seconds to seconds.  [target: 1 line]
    (((h * 60) + m)*60) + s  

let toTime s = //Create a function toTime to convert a number of seconds to hours, minutes, and seconds  [target: 7 lines]
    let s = match s<0 with | true -> 0 | _ -> s //technically a shadowed s here, but uses same value as before. [shadowed s1]
    let h = match s%3600 = 0 with |true -> s/3600 | _ ->  match s%3600>=1 with |true -> s/3600 |_ -> 0 
    let s = s-(h*3600)//shadowed s2
    let m =  match s%60 = 0 with |true -> s/60 | _ ->  match s%60>=1 with |true -> s/60 |_ -> 0 
    let s = s-(m*60)//uses shadowed s2 to determine shadowed s3
    h,m,s//usses shadowed s3 here

let digits x = //Create a function digits to count the number of digits in a number. The input may be positive or negative  [target: 5 lines]
    let rec countDigits v acc = //v = value, acc = accumulator param 
     match v >= 0 with
      |true -> match (v < 10) with |true -> acc + 1 |_ -> countDigits (v/10) (acc+1)
      | _ -> match (v > -10 && v < 0) with |true -> acc + 1 |_ -> countDigits (v/10) (acc+1)
    match x with _ -> countDigits x 0

let minmax (a:int,b:int,c:int,d:int) = //create a function minmax which finds the largest and smallest values out of four values that are provided  [target: 3 lines]
    let Maximum = max a b |> max c |> max d 
    let Minimum = min a b |> min c |> min d 
    Minimum,Maximum

let isLeap y = //returns true if the given year is a leap year. Every year that is divisible by 4 is a leap year, unless it is also divisible by 100.
              //However, if it is also divisible by 400, then it is still a leap year. The function should throw an exception if the input year is less than 1582. [target: 5 lines]
    match y<1582 with |true -> failwith "Year cannot be less than 1582" |_ -> match y%4=0 with
      |false -> false //not divisbile by 4
      |_ -> match y%100=0 with
        |false -> true //divisible by 4 & not divisble by 100
        |_-> match y%400 =0 with |true -> true |_-> false

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