namespace Starikov
open System
open System.Collections

module Commands =
    open System.ComponentModel
    (*let BuildAccount = 
        let account = new EnteredAccount()
        account.IsEnter <- true
        //AccountContainer.AddNewEnteredAccount account
        account*)
    
    open System
    open Starikov.dbModels
    let Setter target values =
        let Set (pr_info: Reflection.PropertyInfo) value =
            try
                pr_info.SetValue(target, value)
            with
                | _ -> 
                        let converter = TypeDescriptor.GetConverter(pr_info.PropertyType)
                        if converter |> isNull |> not then
                            try
                                let value' = converter.ConvertFromString(value.ToString())
                                pr_info.SetValue(target, value')
                            with
                                | _ -> printfn "Bad instance with Property \"%s\"\tValue: %O\tT: %O" pr_info.Name value target
        let T = target.GetType()
        let pi = T.GetProperties()
        if pi.Length <> (Array.length <| values) then failwith "Error! Setter_arguments"
        for i = 0 to pi.Length - 1 do
            Set pi.[i] values.[i]
        target
    let TypeSetter (tp: System.Type) values =
        let target = Activator.CreateInstance(tp)
        let Set (pr_info: Reflection.PropertyInfo) value =
            try
                pr_info.SetValue(target, value)
            with
                | _ ->  
                        let converter = TypeDescriptor.GetConverter(pr_info.PropertyType)
                        if converter |> isNull |> not then
                            try
                                let value' = converter.ConvertFromString(value.ToString())
                                pr_info.SetValue(target, value')
                            with
                                | _ -> printfn "Bad instance with Property \"%s\"\tValue: %O\tT: %s" pr_info.Name value tp.Name
        let pi = tp.GetProperties()
        if pi.Length <> (Array.length <| values) then failwith "Error! Setter_arguments"
        for i = 0 to pi.Length - 1 do
            Set pi.[i] values.[i]
        target       
    let Getter target =
        let t = target.GetType()
        let pri = t.GetProperties()
        [| for pi in pri do yield (pi.Name, pi.GetValue(target)) |]

    let ConvertDate = (fun (t: string) ->
        let hls = t.Split([|' '|])
        let dmy, tm = hls.[0], hls.[1]
        let ymd = dmy.Split([|'.'|]) |> Array.rev |> Array.fold (fun acc t -> sprintf "%s%s-" acc t) "" |> (fun x -> x.Substring(0, x.Length - 1))
        sprintf "%s %s" ymd tm
    )
    let CopyObj (target: 'T) =
        let t = typeof<'T>
        let neo = Activator.CreateInstance(t) :?> 'T
        let values = Getter target |> Array.map (fun (_, v) -> v)
        target, Setter neo values
    let Equal (old: 'T) (neo: 'T) =
        //let T = typeof<'T>
        let primus, secundus = Getter old, Getter neo
        let length = Array.zip primus secundus |> Array.filter (fun ((_, ov), (_, nv)) -> ov <> nv) |> Array.length
        match length with 0 -> true | _ -> false
    let SC man_id (a: 'T, dt: 'T) =    //SmartChecker
        if a <> Unchecked.defaultof<'T> && a <> dt then 
            printfn "For_Account(%s):\tOld value: %O\t->\tNew value: %O" man_id dt a
            true
        else false
    let permission predicate sequence =
        let rec worker t f (s: seq<'a>) = s |> function
            | sq when sq |> Seq.isEmpty -> t, f
            | sq -> 
                let h, tl = Seq.head sq, Seq.tail sq
                if h |> predicate then 
                    worker (seq {yield! t; yield h}) f tl
                else
                    worker t (seq {yield! f; yield h}) tl
        worker Seq.empty Seq.empty sequence
    let EntityToString entity =
        // let T = entity.GetType()
        let t = Getter entity
        t |> Array.filter (fun (_, v) -> v |> isNull |> not) |> Array.fold (fun state (n, v) -> sprintf "%s{\"%s\" - \"%O\"}" state n v) ""
    let IsStudent (claims: seq<Security.Claims.Claim>) = claims |> Seq.tryFind (fun c -> c.Type = "PSTU_Role") |> (fun claim -> if claim.IsNone then false else claim.Value.Value = "student")
    let IsCurator (claims: seq<Security.Claims.Claim>) = claims |> Seq.tryFind (fun c -> c.Type = "PSTU_Role") |> (fun claim -> if claim.IsNone then false else claim.Value.Value = "curator")
    //let ShowClaims (claims: seq<Security.Claims.Claim>) = claims |> Seq.iter (fun c -> printfn "Type: \"%s\"\tValueType: \"%s\"\tValue: \"%s\"" c.Type c.ValueType c.Value)
module Checker =
    open NickBuhro.Translit
    let K s1 s2 =
        let c =
            let W f s =
                let fl, sl = f |> Seq.length, s |> Seq.length
                let rec w big sml acc count =
                    if count > 0 then w (Seq.tail big) sml ((Seq.zip big sml) :: acc) (count - 1) else acc
                if fl > sl then w f s [] (1 + fl - sl) |> Seq.map (fun sq -> sq |> Seq.fold (fun acc (c1, c2) -> if c1 = c2 then (acc + 1) else acc ) 0) |> Seq.max
                else w s f [] (1 + sl - fl) |> Seq.map (fun sq -> sq |> Seq.fold (fun acc (c1, c2) -> if c1 = c2 then (acc + 1) else acc ) 0) |> Seq.max
            W s1 s2
        let a, b = s1 |> Seq.length, s2 |> Seq.length
        ((float <| c) / (float <| (a + b - c)))(*, (c % (a + b - c))*)
    let ToLatin (t: string) = Transliteration.CyrillicToLatin((t.ToLower()) , Language.Russian)
    let Choose token dic = 
        let W (t: string) = 
            let spl = t.Split([|' '; '-'; '.'; ','; '_'|])
            if spl.Length >= 2 then spl.[0], spl.[1] else t.Substring(0, t.Length - 3), t.Substring(t.Length - 3, 3)
        let tg, ty = token |> W
        let modern_dic = dic |> List.map (W)
        let iter_1 = modern_dic |> List.mapi (fun i (_, y) -> (K (ty |> ToLatin) (y |> ToLatin)), i)
        let ny = iter_1 |> List.maxBy (fun (p, _) -> p) |> (fun (p, _) -> p)
        let iter_1' = iter_1 |> List.filter (fun (p, _) -> p = ny) |> List.map (fun (_, i) -> i)
        let modern_dic' = modern_dic |> List.mapi (fun i (g, y) -> (i, g, y)) |> List.filter (fun (i, _, _) -> iter_1' |> List.contains i)
        let iter_2 = modern_dic' |> List.map (fun (i, g, _) -> (K (tg |> ToLatin) (g |> ToLatin)), i)
        let ng = iter_2 |> List.maxBy (fun (p, _) -> p) |> (fun (p, _) -> p)
        let iter_2' = iter_2 |> List.filter (fun (p, _) -> p = ng) |> List.map (fun (_, i) -> i)
        dic.[(List.head <| iter_2')]
module QueryBuilderHelper =
    let GetProperties t =
        let T = t.GetType()
        T.GetProperties() :> seq<System.Reflection.PropertyInfo>
    let GetValues t =
        let T = t.GetType()
        T.GetProperties() |> Seq.map (fun pi -> pi.GetValue(t :> obj))
    let GetPropertiesAndValues t =
        Seq.zip (GetProperties t) (GetValues t)
    let GetID t =
        GetPropertiesAndValues t
        |> Seq.map (fun (pi, v) -> pi.Name, v)
        |> Seq.filter (fun (n, v) -> n.StartsWith("id_"))
        |> Seq.head
    let GetUpdateCommand (deft: 'T) (targ: 'T) =
        let dl', tl' = GetPropertiesAndValues <| deft, GetPropertiesAndValues <| targ
        let rec builder (values: string) (logs: string list) (dl: seq<System.Reflection.PropertyInfo * obj>, tl: seq<System.Reflection.PropertyInfo * obj>) = 
            match dl, tl with
            | s1, s2 when Seq.isEmpty <| s1 && Seq.isEmpty <| s2 -> values, logs
            | d, t ->
                let (dpi, dv), (_, tv) = Seq.head <| d, Seq.head <| t
                if dv = tv || tv |> isNull then builder values logs (Seq.tail <| d, Seq.tail <| t)
                else builder (sprintf "%s,`%s`='%O'" values dpi.Name tv) ((sprintf "CHANGE IN `%s`: FROM '%O' TO '%O'" dpi.Name dv tv) :: logs) (Seq.tail <| d, Seq.tail <| t)
        let ret1, logs = builder "" [] (dl', tl')
        ret1.Substring(1), logs
    //let rec W acc = function | s when Seq.isEmpty <| s -> acc | s -> W ((Seq.head <| s) :: acc) (Seq.tail <| s)
module QueryBuilder =
    let BuildUpdateQuery old neo =
        let values, Logs = QueryBuilderHelper.GetUpdateCommand old neo
        let T = old.GetType()
        let table = T.Name
        let id_name, id_value = QueryBuilderHelper.GetID old
        let query, logs = (sprintf "UPDATE `%s` SET %s WHERE `%s`='%O'" table values id_name id_value), (Logs |> List.fold (fun st s -> sprintf "%s{%s}" st s ) (sprintf "{|%s|}" table))
        printfn "Buildered query: {%s}" query
        query, logs
    let BuildDeleteQuery entity =
        let T = entity.GetType()
        let logs = QueryBuilderHelper.GetPropertiesAndValues entity |> Seq.fold (fun st (pi, v) -> sprintf "%s{`%s`: '%O'}" st pi.Name v ) (sprintf "{|%s|}" T.Name)
        let id_name, id_value = QueryBuilderHelper.GetID entity
        (sprintf "DELETE FROM `%s` WHERE `%s`='%O'" T.Name id_name id_value), logs
    let BuildInsertQueryWithAI entity =
        let T = entity.GetType()
        let table = T.Name.ToLowerInvariant()
        let names, values = Commands.Getter entity |> Array.filter (fun (_, v) -> v |> isNull |> not) |> Array.map (fun (n, v) -> (("`" + n + "`"), sprintf "'%O'" v)) |> Array.unzip
        let fNames = names.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        let fValues = values.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") "" |> sprintf "(%s)"
        table, (sprintf "(%s)" fNames), fValues
    let BuildInsertQueryWithoutAI entity =
        let T = entity.GetType()
        let table = T.Name.ToLowerInvariant()
        let names, values = Commands.Getter entity |> Array.filter (fun (_, v) -> v |> isNull |> not) |> Array.map (fun (n, v) -> (("`" + n + "`"), sprintf "'%O'" v)) |> Array.unzip
        let fNames = names |> Array.tail |> Array.fold (sprintf "%s, %s") (names |> Array.head |> sprintf "%O") |> sprintf "(%s)" 
        let fValues = values |> Array.tail |> Array.fold (sprintf "%s, %s") (values |> Array.head |> sprintf "%O") |> sprintf "(%s)"
        table, fNames, fValues
module Logs =
    open System
    open System.IO

    let _logDirectory = Path.Combine(AppContext.BaseDirectory, "logs")
    let _logFile = Path.Combine(_logDirectory, (sprintf "%d_%d_%d.txt" System.DateTime.Now.Year System.DateTime.Now.Month System.DateTime.Now.Day ))
    let add logText =
        if not <| Directory.Exists(_logDirectory) then Directory.CreateDirectory(_logDirectory) |> ignore
        if not <| File.Exists(_logFile) then File.Create(_logFile).Dispose()
        File.AppendAllText(_logFile, (sprintf "%s\n" logText))
        printfn "%s" logText
    let add_Log msg =
        let logText = sprintf "%s: <|%A|>" (System.DateTime.UtcNow.ToString("hh:mm:ss")) msg
        add logText
    let add_ExtLog query logs =
        let logText = sprintf "%s: <|%s|> <|%A|>" (System.DateTime.UtcNow.ToString("hh:mm:ss")) logs query
        add logText
module Files =
    open System
    let files_extensions = [ (".txt", "text/plain"); (".pdf", "application/pdf"); (".doc", "application/vnd.ms-word"); (".docx", "application/vnd.ms-word"); (".xls", "application/vnd.ms-excel"); (".xlsx", "application/vnd.openxmlformatsofficedocument.spreadsheetml.sheet"); (".png", "image/png"); (".jpg", "image/jpeg"); (".jpeg", "image/jpeg"); (".gif", "image/gif"); (".csv", "text/csv") ]
    let GetFileExt path =
        let ext = System.IO.Path.GetExtension(path).ToLowerInvariant()
        files_extensions |> List.filter (fun (e, _) -> e = ext) |> List.head |> fun (_, r) -> r
module MessagerModule =
    open MimeKit
    open MailKit.Net.Smtp
    let Send (fromEmail, fromPass) message_subject message_body (sendTo: seq<string * string>) =
        let message = new MimeMessage()
        message.From.Add(new MailboxAddress("БФ ПНИПУ сообщение сайта \"Дневник куратора\"", fromEmail))
        sendTo |> Seq.iter (fun (name, email) -> message.To.Add(new MailboxAddress(name, email)))
        message.Subject <- message_subject
        message.Body <- new TextPart("plain", Text = message_body)
        use client = new SmtpClient()
        client.Connect("smtp.gmail.com", 587, false)
        client.Authenticate(fromEmail, fromPass)
        client.Send(message)
        client.Disconnect(true) 
module JsonModule =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open System
    open System.IO
    let GetBody (path: string) =
        use reader = new StreamReader(path)
        reader.ReadToEnd()
    type JSON (body: string) =
        class
            member val Token = JToken.Parse(body) with get, set
            member self.Item (keys: string) =
                let ks = keys.Split([|';'; ':'|]) |> Array.toList
                let rec worker (jt: JToken) (id: string list) = 
                    match id with
                    | [] -> jt.Value<obj>()
                    | h::t -> worker jt.[h] t
                worker self.Token ks
        end