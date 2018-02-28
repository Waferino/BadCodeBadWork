namespace Starikov
module Commands =
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
                | _ -> printfn "Bad instance with Property \"%s\"\tT: %A" pr_info.Name target
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
                | _ -> printfn "Bad instance with Property \"%s\"\tT: %s" pr_info.Name tp.Name
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
    let SC man_id (a: 'T, dt: 'T) =    //SmartChecker
        if a <> Unchecked.defaultof<'T> && a <> dt then 
            printfn "For_Account(%s):\tOld value: %O\t->\tNew value: %O" man_id dt a
            true
        else false
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
        (sprintf "UPDATE `%s` SET %s WHERE `%s`='%O'" table values id_name id_value), (Logs |> List.fold (fun st s -> sprintf "%s{%s}" st s ) (sprintf "{|%s|}" table))
    let BuildDeleteQuery entity =
        let T = entity.GetType()
        let logs = QueryBuilderHelper.GetPropertiesAndValues entity |> Seq.fold (fun st (pi, v) -> sprintf "%s{`%s`: '%O'}" st pi.Name v ) (sprintf "{|%s|}" T.Name)
        let id_name, id_value = QueryBuilderHelper.GetID entity
        (sprintf "DELETE FROM `%s` WHERE `%s`='%O'" T.Name id_name id_value), logs
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