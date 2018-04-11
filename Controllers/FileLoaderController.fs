namespace Starikov.Controllers

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
//open System.ComponentModel.DataAnnotations
open Starikov
open Starikov.dbModels
open Microsoft.Extensions.DependencyModel.Resolution
open Microsoft.AspNetCore.Mvc.ModelBinding
open Microsoft.AspNetCore.Http.Extensions


type FileLoaderController (context: IBaseSQLCommands) =
    inherit Controller()
    member val ctx = context with get
    //[<Authorize>]
    member this.Index () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        let file_dir = Path.Combine(Directory.GetCurrentDirectory(), "wwwroot", "uploading_files")
        let dir_info = new DirectoryInfo(file_dir)
        let files = dir_info.GetFiles() |> Array.map (fun fi -> fi.Name)
        this.ViewData.["files"] <- files
        this.View()
    member this.UploadFile () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.View()
    [<HttpPost>]
    member this.UploadFile (file: IFormFile) =
        if file |> isNull || file.Length = 0L then 
            printfn "file not selected"
            this.RedirectToAction("Index")
        else
            let path = Path.Combine(Directory.GetCurrentDirectory(), "wwwroot", "uploading_files", file.FileName)
            use stream = new FileStream(path, FileMode.Create)
            file.CopyTo(stream)
            this.RedirectToAction("Index")
    member this.GetFile (filename) = //
        let path = Path.Combine(Directory.GetCurrentDirectory(), "wwwroot", "uploading_files", filename) // <- filename
        let memory = new MemoryStream()
        use stream = new FileStream(path, FileMode.Open)
        stream.CopyTo(memory)
        memory.Position <- 0L
        let contentType, fname = Files.GetFileExt path, Path.GetFileName(path)
        this.File(memory, contentType, fname)