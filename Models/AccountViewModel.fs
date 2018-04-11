namespace Starikov

open System
open Microsoft.AspNetCore
open System.ComponentModel.DataAnnotations

type LoginViewModel () =
    member val Login : string = null with get, set

    member val Identity : string = null with get, set
type RegisterViewModel () =
    member val Lastname : string = null with get, set
    member val Firstname : string = null with get, set
    member val Midlename : string = null with get, set
    member val Number : string = null with get, set
    //[<Required>]
    //[<EmailAddress>]
    //[<Display(Name = "Email")>]
    member val Email : string = null with get, set
    //[<Required>]
    //[<StringLength(100, ErrorMessage = "The {0} must be at least {2} and at max {1} characters long.", MinimumLength = 5)>]
    //[<DataType(DataType.Password)>]
    //[<Display(Name = "Password")>]
    member val Password : string = null with get, set
    //[<DataType(DataType.Password)>]
    //[<Display(Name = "Confirm password")>]
    //[<Compare("Password", ErrorMessage = "The password and confirmation password do not match.")>]
    member val rePassword : string = null with get, set
    member val SpecialWord : string = null with get, set