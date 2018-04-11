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
open Starikov.dbModels
open Microsoft.Extensions.DependencyModel.Resolution
open Microsoft.AspNetCore.Mvc.ModelBinding
open Microsoft.AspNetCore.Http.Extensions
open System.Reflection.Metadata
open System.IO


type FunctionsController (context: IMyDBContext, mes: IMessager) =
    inherit Controller()
    member val ctx = context with get
    member val messager = mes with get
    //[<Authorize>]
    member this.GroupInfo () =
        let isAuth = this.User.Identity.IsAuthenticated
        this.ViewData.["IsAuthenticated"] <- isAuth
        if isAuth then 
            this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
            let isCur = Commands.IsCurator this.User.Claims
            this.ViewData.["IsCurator"] <- isCur
            this.ViewData.["UserId"] <- this.User.Identity.Name
        let groups = this.ctx.GetGroups
        this.View(groups)
    member this.OneGroupInfo (id : int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["Group_name"] <- this.ctx.GetOneGroup id |> Option.map (fun g -> g.name_group) |> Option.get
        let students = 
            this.ctx.GetGroupStudents id
            |> Seq.map (fun s ->
                                        let acc = this.ctx.GetAccount ((s.id_man).ToString())
                                        let personVal = 
                                            acc.Person 
                                            |> Commands.Getter 
                                            |> Array.zip ((acc.Person :> ICafedraEntities).GetNamesOfProperties())
                                            |> Array.Parallel.map (fun (n, (f, s)) -> new CSharpDuoTurple(PrName = n, PrRealName = f, PrValue = s))
                                        let studVal = 
                                            acc.Student 
                                            |> Commands.Getter 
                                            |> Array.map (fun (n, v) -> 
                                                                        if n <> "id_group" then 
                                                                            (n, v)
                                                                        else
                                                                            let v' = v |> this.ctx.GetOneGroup |> Option.map (fun gr' -> gr'.name_group :> obj)
                                                                            if v' |> Option.isSome then (n, (v' |> Option.get))
                                                                            else (n, ("-" :> obj)))
                                            |> Array.zip ((acc.Student :> ICafedraEntities).GetNamesOfProperties()) 
                                            |> Array.tail 
                                            |> Array.Parallel.map (fun (n, (f, s)) -> new CSharpDuoTurple(PrName = n, PrRealName = f, PrValue = s))
                                        Array.concat (seq { yield personVal; yield studVal}) )
            |> Seq.map (fun a -> 
                                        if Commands.IsCurator <| this.User.Claims then a
                                        else [| a.[1]; a.[2]; a.[3]; a.[10]; a.[26]; a.[27]; a.[59]; a.[61] |] )                            
        if this.User.Identity.IsAuthenticated then 
            let isCurator = Commands.IsCurator this.User.Claims
            this.ViewData.["IsCurator"] <- isCurator
        this.View(students)
    member this.StudentInfo () =    // VERY SLOW!!! NEED PARALLELING, BUT PROBLEMS IN DB ACCESS 
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        let students = 
            this.ctx.GetStudents 
            |> Seq.map (fun s ->
                                        let acc = this.ctx.GetAccount ((s.id_man).ToString())
                                        let personVal = 
                                            acc.Person 
                                            |> Commands.Getter 
                                            |> Array.zip ((acc.Person :> ICafedraEntities).GetNamesOfProperties())
                                            |> Array.Parallel.map (fun (n, (f, s)) -> new CSharpDuoTurple(PrName = n, PrRealName = f, PrValue = s))
                                        let studVal = 
                                            acc.Student 
                                            |> Commands.Getter 
                                            |> Array.map (fun (n, v) -> 
                                                                        if n <> "id_group" then 
                                                                            (n, v)
                                                                        else
                                                                            let v' = v |> this.ctx.GetOneGroup |> Option.map (fun gr' -> gr'.name_group :> obj)
                                                                            if v' |> Option.isSome then (n, (v' |> Option.get))
                                                                            else (n, ("-" :> obj)))
                                            |> Array.zip ((acc.Student :> ICafedraEntities).GetNamesOfProperties()) 
                                            |> Array.tail 
                                            |> Array.Parallel.map (fun (n, (f, s)) -> new CSharpDuoTurple(PrName = n, PrRealName = f, PrValue = s))
                                        Array.concat (seq { yield personVal; yield studVal}) )
            |> Seq.map (fun a -> 
                                        if Commands.IsCurator <| this.User.Claims then a
                                        else [| a.[1]; a.[2]; a.[3]; a.[10]; a.[26]; a.[27]; a.[59]; a.[61] |] )
        this.View(students)
    member this.EventsInfo () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["IsStudent"] <- this.User.Claims |> Commands.IsStudent
        if this.User.Identity.IsAuthenticated then this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        let EventsInfos = this.ctx.GetEventsInfos
        let retD = EventsInfos |> Seq.filter (fun ei -> String.IsNullOrEmpty(ei.DateOfThe) |> not) |> Seq.map (fun ei -> DateTime.Parse(ei.DateOfThe), ei) |> Seq.sortBy (fun (d, _) -> d) |> Seq.map (fun (_, ei) -> ei)
        let retU = EventsInfos |> Seq.filter (fun ei -> String.IsNullOrEmpty(ei.DateOfThe)) |> Seq.sortBy (fun ei -> ei.Name)
        let ret = seq { for ei in retD -> ei
                        for ei in retU -> ei }
        if this.User.Identity.IsAuthenticated then
            let mid, gid = this.ctx.GetAccount this.User.Identity.Name |> (fun a -> (a.Person.id_man, (if a.IsStudent then (if a.Student.id_group.HasValue then a.Student.id_group.Value else 0) else 0) ) )
            let ct = this.ctx :?> IBaseSQLCommands
            let evts = ct.Get (new Starikov.dbModels.Event())
            this.ViewData.["meEvents"] <- evts |> Seq.filter (fun e -> if e.isGroup_Event then e.fk_student_or_group = gid else e.fk_student_or_group = mid) |> Seq.map (fun e -> printfn "id_EI: %A"; e.id_EventInfo)
            this.ViewData.["groupEvents"] <- evts |> Seq.filter (fun e -> e.isGroup_Event) |> Seq.filter (fun e -> e.fk_student_or_group = gid) |> Seq.map (fun e -> printfn "id_EI: %A"; e.id_EventInfo)
        this.View(ret)
    [<Authorize>]
    member this.CreateEvent () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["id_man"] <- this.User.Identity.Name |> int
        this.ViewData.["IsCurator"] <- this.User.Claims |> Commands.IsCurator
        this.View(new Starikov.dbModels.EventInfo(whosEvent = "Студентов"))
    [<Authorize>]
    [<HttpPost>]
    member this.CreateEvent (event: EventInfo) = //INSERT INTO `www0005_base`.`eventinfo` (`DateOfThe`, `Name`) VALUES ('2010.11.10', 'Поход на лыжах');
        let sei = this.ctx.GetEventsInfos |> Seq.filter (fun ei -> ei.id_EventInfo = event.id_EventInfo) |> Seq.tryHead
        event.creatingDate <- DateTime.Now.ToString("MM-dd-yyyy")
        if sei.IsNone then
            let res = this.ctx.InsertEventInfo event
            res |> printfn "Event(\"%s\") is created? <%b>" event.Name
        else 
            let ei = sei.Value
            let query, logs = QueryBuilder.BuildUpdateQuery ei event
            (this.ctx :?> IBaseSQLCommands).Execute query logs |> printfn "Event(\"%s\") is updated? <%s>" event.Name
        //if res then printfn "Event: \"%s\" was inserted" event.Name else printfn "Broken inserting Event: \"%s\"" event.Name
        this.RedirectToAction("EventsInfo")
    [<Authorize>]
    member this.EditEventInfo (id: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["id_man"] <- this.User.Identity.Name |> int
        this.ViewData.["IsCurator"] <- this.User.Claims |> Commands.IsCurator
        let ei = this.ctx.GetEventsInfos |> Seq.filter (fun ei -> ei.id_EventInfo = id) |> Seq.head
        this.View(ei)
    [<Authorize>]
    member this.RemoveEventInfo (id: int) =
        let res = this.ctx.GetEventsInfos |> Seq.filter (fun ei -> ei.id_EventInfo = id) |> Seq.head |> this.ctx.Remove
        this.RedirectToAction("EventsInfo")
    [<Authorize>]
    member this.ChooseGroup (id_ei: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["id_EventInfo"] <- id_ei
        let groups = this.ctx.GetGroups
        let model = if this.User.Claims |> Commands.IsCurator then groups |> Seq.filter (fun g -> g.kurator = this.User.Identity.Name) else groups
        this.View(model)
    [<Authorize>]
    member this.ChooseEvent (id_gr: int) =
        this.View()
    [<Authorize>]
    member this.CheckInEvent (id_gr: int, id_ei: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["id_man"] <- this.User.Identity.Name |> int
        if id_gr <> 0 then this.ViewData.["isGE"] <- true else this.ViewData.["isGE"] <- false
        this.ViewData.["id_Group"] <- id_gr
        this.ViewData.["id_EventInfo"] <- id_ei
        let model = new Starikov.dbModels.Event()
        this.View(model)
    (*[<Authorize>]
    member this.CheckInEvent (id_ei: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["isGE"] <- false
        this.ViewData.["id_EventInfo"] <- id_ei
        let model = new Starikov.dbModels.Event(id_EventInfo = id_ei)
        this.View(model)*)
    [<Authorize>]
    [<HttpPost>]
    member this.CheckInEvent (event: Starikov.dbModels.Event) =
        let isStudent = Commands.IsStudent <| this.User.Claims
        let id_man = this.User.Identity.Name |> int
        event.creatingDate <- (System.DateTime.Now.ToString("MM-dd-yyyy"))
        let res = this.ctx.InsertEvent event
        printfn "RESULT: %A" res
        this.RedirectToAction("EventsInfo")
    [<Authorize(Policy = "StudentOnly")>]
    member this.ManageEvent (id_ei: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        let event = (this.ctx :?> IBaseSQLCommands).Get (new Starikov.dbModels.Event()) |> Seq.filter (fun e -> e.id_EventInfo = id_ei && e.fk_student_or_group = (this.User.Identity.Name |> int)) |> Seq.head
        let ei = this.ctx.GetEventsInfos |> Seq.filter (fun ei -> ei.id_EventInfo = event.id_EventInfo) |> Seq.head
        this.ViewData.["EI_Name"] <- ei.Name
        this.ViewData.["EI_Notation"] <- ei.Notation
        this.View(event)
    [<Authorize(Policy = "StudentOnly")>]
    member this.AddExtraEvent (id_event: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        //let model = new ExtraEvent(id_Event = id_event)
        this.ViewData.["id_Event"] <- id_event
        this.View()
    [<Authorize(Policy = "StudentOnly")>]
    [<HttpPost>]
    member this.AddExtraEvent (file: IFormFile, id_event: int) =
        if file |> isNull || file.Length = 0L then 
            printfn "file not selected"
            this.RedirectToAction("AddExtraEvent", id_event)
        else
            let path = Path.Combine(Directory.GetCurrentDirectory(), "wwwroot", "ExtraEvents_Students_files", this.User.Identity.Name, id_event.ToString(), file.FileName)
            let directory_name = Path.GetDirectoryName(path)
            let directory = new DirectoryInfo(directory_name)
            if directory.Exists |> not then Directory.CreateDirectory(directory_name) |> ignore
            let fileInfo = new FileInfo(path)
            if fileInfo.Exists |> not then fileInfo.Delete()
            use stream = new FileStream(path, FileMode.Create)
            file.CopyTo(stream)
            let extraEvent = new ExtraEvent(id_Event = id_event, fileName = file.FileName, contentType = file.ContentType, fileDataPath = path, creatingDate = DateTime.Now.ToString("MM-dd-yyyy"))
            let res = this.ctx.InsertExtraEvent <| extraEvent
            printfn "ExtraEvent insert result: <%b>" res
            this.RedirectToAction("EventsInfo")
    [<Authorize(Policy = "StudentOnly")>]
    member this.CheckOutEvent (id_event: int) =
        let res = (this.ctx :?> IBaseSQLCommands).Get (new Starikov.dbModels.Event()) |> Seq.filter (fun e -> e.id_Event = id_event) |> Seq.head |> this.ctx.Remove
        let path = Path.Combine(Directory.GetCurrentDirectory(), "wwwroot", "ExtraEvents_Students_files", this.User.Identity.Name, id_event.ToString())
        let di = new DirectoryInfo(path)
        if di.Exists then
            di.GetFiles() |> Array.iter (fun fi -> fi.Delete())
            di.Delete()
        this.RedirectToAction("EventsInfo")
    [<Authorize>]
    member this.StudentAnceta () =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["Groups"] <- this.ctx.GetGroups |> Seq.map (fun g -> g.name_group)
        this.ViewData.["Languages"] <- (this.ctx :?> IBaseSQLCommands).Get (new LanguageDictionary()) |> Seq.map (fun lang -> lang.langName)
        this.ViewData.["Family_Status"] <- this.ctx.GetAccount this.User.Identity.Name |> fun ai -> ai.Person.pol |> function "1" -> ["Женат"; "Холост"] | "2" -> ["Замужем"; "Не замужем"] | _ -> ["Женат"; "Холост"; "Замужем"; "Не замужем"]
        this.View(this.ctx.GetAnceteData <| this.User.Identity.Name)
    [<Authorize>]
    [<HttpPost>]
    member this.StudentAnceta (ancet: Anceta) =
        let res = this.ctx.SetAnceteData this.User.Identity.Name ancet
        if res |> not then 
            printfn "При обработке анкеты(%s) возникли проблеммы" ( sprintf "%s %c.%c." ancet.lastname ancet.name.[0] ancet.patron.[0] )
            this.RedirectToAction("Functions", "StudentAnceta", ancet)
        else
            this.RedirectToAction("Index", "Home")
    [<Authorize(Policy = "CuratorOnly")>]
    member this.MessageToGroup (id_group: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        this.ViewData.["id_group"] <- id_group
        this.View(new MessageToGroupModel(id_group = id_group))
    [<Authorize(Policy = "CuratorOnly")>]
    [<HttpPost>]
    member this.MessageToGroup (model: MessageToGroupModel) =
        let students = 
            let s = this.ctx.GetStudents |> Seq.filter (fun st -> st.id_group.HasValue && st.id_group.Value = model.id_group)
            if Seq.length <| s > 0 then s |> Seq.map (fun st -> st.id_man) else Seq.empty
        if students |> Seq.isEmpty |> not then
            let peoples_data = this.ctx.GetPeoples |> Seq.filter (fun p -> (Seq.contains p.id_man students)) //|> Seq.map (fun p -> ((sprintf "%s %c.%c." p.fam p.name.[0] p.otchestvo.[0]), p.id_man))
            let accs_data = (this.ctx :?> IBaseSQLCommands).GetFiltered (fun (a: dbModels.Account) -> peoples_data |> Seq.map (fun p -> p.id_man) |> Seq.contains a.id_man) |> Seq.map (fun a -> peoples_data |> Seq.filter (fun p -> p.id_man = a.id_man) |> Seq.head |> fun p -> ((sprintf "%s %c.%c." p.fam p.name.[0] p.otchestvo.[0]), a.Email))
            if accs_data |> Seq.isEmpty |> not then
                this.messager.SendMessage model.message_subject model.message_body accs_data
            else
                printfn "Error in message to group(%d): missing e_mails" model.id_group
        else
            printfn "Error in message to group(%d): missing students" model.id_group
        this.RedirectToAction("Index", "Home")
    [<Authorize(Policy = "CuratorOnly")>]
    member this.MessageToStudent (id_student: int) =
        this.ViewData.["IsAuthenticated"] <- this.User.Identity.IsAuthenticated
        this.ViewData.["FIO"] <- this.ctx.GetFIO this.User.Identity.Name
        //printfn "id_student in \"MessageToStudent\": %A" id_student
        this.ViewData.["id_student"] <- id_student
        this.View(new MessageToStudentModel(id_student = id_student))
    [<Authorize(Policy = "CuratorOnly")>]
    [<HttpPost>]
    member this.MessageToStudent (model: MessageToStudentModel) =
        let student = this.ctx.GetPeoples |> Seq.filter (fun p -> p.id_man = model.id_student) |> Seq.tryHead
        if student.IsSome then
            let acc = (this.ctx :?> IBaseSQLCommands).GetFiltered (fun (a: dbModels.Account) -> a.id_man = student.Value.id_man) |> Seq.tryHead
            if acc.IsSome then
                let student_data = seq { yield (this.ctx.GetFIO student.Value.id_man, acc.Value.Email) }
                this.messager.SendMessage model.message_subject model.message_body student_data
            else
                printfn "Error in message to student(%d): missing e_mails" model.id_student
        else
            printfn "Error in message to student(%d): missing students" model.id_student
        this.RedirectToAction("Index", "Home")