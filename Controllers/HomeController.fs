namespace Starikov.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
//open System.ComponentModel.DataAnnotations
open Starikov
open Microsoft.Extensions.DependencyModel.Resolution
open Microsoft.AspNetCore.Mvc.ModelBinding


type HomeController (context: IBaseSQLCommands) =
    inherit Controller()
    member val ctx = context with get
    //[<Authorize>]
    member this.Index () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        //if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["IsStudent"] <- this.User.Claims |> Commands.IsStudent
        if this.User.Identity.IsAuthenticated then 
            let AccountInfo = this.User.Identity.Name |> ATP.GetAccount this.ctx
            this.ViewData.["AccountInfo"] <- AccountInfo
            this.ViewData.["FIO"] <- ATP.GetFIO this.ctx this.User.Identity.Name
        this.View()

    member this.About () =
        this.ViewData.["Message"] <- "Your application description page."
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- ATP.GetFIO this.ctx this.User.Identity.Name
        this.View()
    //[<Authorize(Policy = "CuratorOnly")>] //It's WORK!!!
    member this.Contact () =
        this.ViewData.["Message"] <- "Igor Starikov"
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- ATP.GetFIO this.ctx this.User.Identity.Name
        this.View()

    member this.Error () =
        this.View();
