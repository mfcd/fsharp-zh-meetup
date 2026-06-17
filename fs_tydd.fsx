// Exhaustiveness
// Idea: promote such warnings to errors!
type TrafficLight = 
    | Red 
    | Yellow 
    | Green

let getAction light =
    match light with
    | Red -> "Stop"
    | Green -> "Go"
    // We intentionally forgot Yellow!


// Single case discriminated unions with a private constructor
// private — hides the constructor EmailAddress of string from outside the module. 
// Nobody can write EmailAddress "whatever" directly — they must go through Email.create, which runs the validation.

type EmailAddress = private EmailAddress of string

module Email =

    let create (s: string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s, @"^[^@\s]+@[^@\s]+\.[^@\s]+$")
        then Some (EmailAddress s)
        else None

    let value (EmailAddress s) = s

    let send (email: EmailAddress) =
        printfn "Sending to: %s" (value email)

let sendWelcome input =
    match Email.create input with
    | Some email -> Email.send email
    | None       -> printfn "Invalid email: %s" input

sendWelcome "mattia@entropy42.com"   // Sending to: mattia@entropy42.com
sendWelcome "not-an-email"           // Invalid email: not-an-email


//
// Units of meaure
//
[<Measure>] type USD // US Dollars 
[<Measure>] type EUR // Euros
let accountBalance = 100.0<USD>
let checkAmount = 50.0<EUR> 
let newBalance = accountBalance + checkAmount // ❌ type error: USD + EUR

// Units compose automatically through arithmetic
[<Measure>] type m   // metres
[<Measure>] type s   // seconds
[<Measure>] type kg  // kilograms

let distance = 100.0<m>
let time     =   9.58<s>   // Usain Bolt's 100m world record

let speed    = distance / time    // float<m/s>    — compiler infers the unit
let momentum = 94.0<kg> * speed   // float<kg m/s> — unit tracks through multiplication


//
// Type Providers
//
#r "nuget: FSharp.Data"
open FSharp.Data

// The compiler reads the sample at compile-time and generates a type from it.
// No classes to write, no DTOs, no mapping code.
type GitHubUser = JsonProvider<"""
    {
        "login": "octocat",
        "id": 5832347,
        "public_repos": 8,
        "bio": "Testing"
    }
""">

let displayUserInfo (rawJson: string) =
    let user = GitHubUser.Parse(rawJson)
    // user.Login       is string  ← compiler knows this
    // user.PublicRepos is int     ← compiler knows this
    // user.Id          is int     ← compiler knows this
    printfn "User %s (id=%d) has %d public repos." user.Login user.Id user.PublicRepos


//
// Make invalid states irrepresentable
// 

// Bad!
type Order() = 
    // 'val mutable' defines a changeable field (like C# auto-properties) 
    // The '[<DefaultValue>]' attribute initializes them to false or null 
    
    // States represented by flags 
    [<DefaultValue>] val mutable IsPaid : bool 
    [<DefaultValue>] val mutable IsShipped : bool 
    
    // Data that belongs to specific states, but must live globally on the object 
    [<DefaultValue>] val mutable PaymentMethod : string // e.g., "Cash", "CreditCard" 
    [<DefaultValue>] val mutable CardNumber : string    // Nullable 
    [<DefaultValue>] val mutable CardExpiry : string    // Nullable 
    [<DefaultValue>] val mutable TrackingNumber : string // Nullable

// Better 
type Payment =
| Cash
| CreditCard of number: string * expiry: string

type  Order =
| Cart
| Paid of Payment
| Shipped of trackingNumber: string