module Synthesis

open System.Xml.Linq
open System.Xml.Linq

let abelar a = //return true if value is greater than 12, but less than 3097 and is a multiple of 12, otherwise return false.  [target: 1 line]
    ((12 < a  && a < 3097) && a % 12=0)
        
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
    let s = match s<0 with 
    |true -> 0
    |_ -> s
    let h = s/3600 in
    let m = (s-(h*3600))/60 in
    let s = s - ((h*3600) + (m*60))
    h,m,s

let digits x = //Create a function digits to count the number of digits in a number. The input may be positive or negative  [target: 5 lines]
    let rec countDigits v acc = //v= value, acc = acumulator param
        match ((v < 10 && v >= 0) || (v > -10 && v < 0)) with 
        |true -> acc + 1
        |_ -> countDigits (v/10) (acc+1)
    countDigits x 0

let minmax (a:int,b:int,c:int,d:int) = //create a function minmax which finds the largest and smallest values out of four values that are provided  [target: 3 lines]
    let Maximum = max a b |> max c |> max d 
    let Minimum = min a b |> min c |> min d 
    Minimum,Maximum
   
let isLeap y = //returns true if the given year is a leap year. Every year that is divisible by 4 is a leap year, unless it is also divisible by 100.
              //However, if it is also divisible by 400, then it is still a leap year. The function should throw an exception if the input year is less than 1582. [target: 5 lines]
    match y<1582 with 
    |true -> failwith "Year cannot be less than 1582" 
    |_ -> 
    match ((y%4=0 && (y%100=0 && y%400 =0)) || (y%4=0) && (not (y%100=0))) with
    |true -> true
    |_ -> false

let month = function //accepts an integer between 1 and 12 inclusive, and returns the corresponding month and the number of days in that month, 
                       //assuming that it is not a leap year. If an integer less than 1 or greater than 12 is supplied, an exception should be thrown. [target: 13 lines]
     |1 -> ("January",31)
     |2 -> ("February",28)
     |3 -> ("March",31)
     |4 -> ("April",30)
     |5 -> ("May",31)
     |6 -> ("June",30)
     |7 -> ("July",31)
     |8 -> ("August",31)
     |9 -> ("September",30)
     |10 -> ("October",31)
     |11 -> ("November",30)
     |12 -> ("December",31)
     |_ -> failwith "Cannot be less than 1 or greater than 12"

let toBinary x =  //converts a positive integer to a binary string.  Throw an exception if a negative integer is supplied  [target: 11 lines]
    match x < 0 with 
    |true -> failwith "Cannot be negative"
    |false -> 
    let rec binaryUp x  = 
        match x with 
        |0 |1 -> string x //force it to be a string (if x = 0, then return "0"; if x = 1, then return "1")
        |_ -> (binaryUp(x/2)) + string (x%2)
    binaryUp x 
        
let bizFuzz n = //accept an integer n and return the number of times a number between 1 and n inclusive is 
                //divisible by 3, 
                //divisible by 5, 
                //and divisible by both 3 and 5. [target: 10 lines]
     let rec betweenOneAndN x (acc1,acc2,acc3) = 
        match (x <= n && n >= 1) with
        |false -> (acc1,acc2,acc3)
        |_ -> 
        match (x%3=0,x%5=0,x%15=0) with
        |(_,_,true) -> betweenOneAndN (x+1) (acc1+1,acc2+1,acc3+1)
        |(_, true, false) -> betweenOneAndN (x+1) (acc1,acc2+1,acc3)
        |(true, false , false) -> betweenOneAndN (x+1) (acc1+1,acc2,acc3)
        |(false,false,false) -> betweenOneAndN (x+1) (acc1,acc2,acc3)
     betweenOneAndN 1 (0,0,0)

let monthDay d y = //accepts an integer d and a year y, and returns a string for the month that the day d falls within. 
                  //function must accept a range of d from 1 to 365 if y isn’t a leap year, 
                  //and must accept d between 1 and 366 if y is a leap year. 
                //If d is out of range, or if y is less than 1582, then an exception must be thrown.   [target: 17 lines]
    let rec doTheStuff (lowerTotal:int,upperTotal:int,n:int,leapYear:int) = //use for ranges like (0 < d < 31) or (32 < d 68) etc.
     let (e,f) = match n+1 <= 12 with
     |true ->  month (n+1)
     |_ -> month 12
     let (a,b) = month (n) 
     match (d >= lowerTotal, d <= upperTotal) with //figure this out
    |true,true -> let (w,z) = month n
                  w
    |true, false ->  match n+1 with |2 -> doTheStuff ((lowerTotal + b),((upperTotal+f)+leapYear),n+1,leapYear) |_ -> match n with |3 -> doTheStuff (((lowerTotal+b)+leapYear),(upperTotal+f),n+1,leapYear) | _ -> doTheStuff ((lowerTotal + b),(upperTotal+f),n+1,leapYear)
    |_ ->  failwith ("{failed} lowerTotal = "+ string lowerTotal + ", upperTotal = "+ string upperTotal+".")
    match isLeap y with 
    |true -> match (d<= 366 & d>=1) with //Leap year
        |false ->  failwith "That day is not valid (it is either greater than 366 or less than 1"
        |_ ->  doTheStuff(1,31,1,1) //this is a leap year, it will break if I leave it like this...
    |_ -> match (d <=365 & d >=1) with //Non-leap year 
        |false -> failwith "That day is not valid (it is either greater than 365 or less than 1"
        |_ -> doTheStuff (1,31,1,0)
  
let coord _ =
    failwith "Not implemented"