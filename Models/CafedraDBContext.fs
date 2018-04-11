namespace Starikov

open System
open System.Collections.Generic
open MySql.Data.MySqlClient

open Starikov.dbModels
open MySql.Data
open System.Text.RegularExpressions
open Microsoft.Data.Edm
open Microsoft.AspNetCore.Http.Extensions

type CafedraDBContext(connectionString: string) =
    member val ConnectionString = connectionString with get, set //@"server=localhost;userid=root;password=kagura;persistsecurityinfo=True;database=www0005_base"
    member private __.GetSqlConnection = new MySqlConnection(__.ConnectionString)
    interface IBaseSQLCommands with
        member this.Execute query logs =
            try
                use conn = this.GetSqlConnection
                conn.Open()
                let cmd = new MySqlCommand(query, conn)
                use reader = cmd.ExecuteReader()
                let mutable ret = "*"
                while reader.Read() do 
                    ret <- ret + "*"
                conn.Close()
                Logs.add_ExtLog query logs
                ret
            with
                | _ -> 
                    printfn "QUERY: %s" query
                    sprintf "Ошибка в команде \"%s\"" query
        member this.Get table = 
            let ret = new List<obj []>()
            use conn = this.GetSqlConnection
            conn.Open()
            let cmd = new MySqlCommand((sprintf "SELECT * FROM `%s`.`%s`" conn.Database table), conn)
            use reader = cmd.ExecuteReader()
            let fields = reader.FieldCount
            while reader.Read() do
                //ret.Add(reader.GetString("fam"))
                let mutable internArr = 
                    [|for i = 0 to fields - 1 do
                        yield reader.GetValue(i)|]
                ret.Add(internArr)
            ret :> seq<obj []>
        member this.Get (t: 'T) =
            let tp = t.GetType()
            (this :> IBaseSQLCommands).Get tp.Name
            |> Seq.map (fun v -> (Commands.TypeSetter tp v) :?> 'T)
        member this.GetFromType tp = 
            use conn = this.GetSqlConnection
            let query = sprintf "SELECT * FROM `%s`.`%s`" conn.Database tp.Name
            conn.Open()
            let cmd = new MySqlCommand(query, conn)
            use reader = cmd.ExecuteReader()
            let fields = reader.FieldCount
            try
                let ret = seq { while reader.Read() do yield Commands.TypeSetter tp [| for i = 0 to fields - 1 do yield reader.GetValue(i) |] }
                Some <| (ret :?> seq<'T>)
            with
                | _ -> None
        member this.GetWhere table def =
            let ret = new List<obj []>()
            use conn = this.GetSqlConnection
            conn.Open()
            let cmd = new MySqlCommand((sprintf "SELECT * FROM `%s`.`%s` WHERE %s" conn.Database table def), conn)
            use reader = cmd.ExecuteReader()
            let fields = reader.FieldCount
            while reader.Read() do
                let mutable internArr = 
                    [|for i = 0 to fields - 1 do
                        yield reader.GetValue(i)|]
                ret.Add(internArr)
            ret :> seq<obj []>       
        member this.GetFiltered (predicate: 'T -> bool) =
            let T = Activator.CreateInstance(typeof<'T>) :?> 'T
            (this :> IBaseSQLCommands).Get T |> Seq.filter predicate
        member this.Insert table props data =
            try
                use conn = this.GetSqlConnection
                conn.Open() //"INSERT INTO table (A, B) VALUES (a, b);"
                let query = sprintf "INSERT INTO `%s`.`%s` %s VALUES %s;" conn.Database table props data
                printfn "Insert command: \"%s\"" query
                let cmd = new MySqlCommand(query, conn)
                use reader = cmd.ExecuteReader()
                let mutable ret = "*"
                while reader.Read() do 
                    ret <- ret + "*"
                conn.Close()
                Logs.add_Log query
                ret
            with
                | ex -> raise ex
        member this.Update table data keygen =
            try
                use conn = this.GetSqlConnection
                conn.Open() //"INSERT INTO table (A, B) VALUES (a, b);"
                let query = sprintf "UPDATE `%s`.`%s` SET %s WHERE %s;" conn.Database table data keygen
                let cmd = new MySqlCommand(query, conn)
                use reader = cmd.ExecuteReader()
                let mutable ret = ""
                while reader.Read() do 
                    ret <- ret + "*"
                conn.Close()
                Logs.add_Log query
                ret
            with
                | ex -> ex.Message
        member this.GetPK entity =
            try
                use conn = this.GetSqlConnection
                conn.Open()
                let query = sprintf "SELECT max(%s) FROM `%s`.`%s`;" (entity.GetProperties().[0]).Name conn.Database entity.Name
                let cmd = new MySqlCommand(query, conn)
                use reader = cmd.ExecuteReader()
                let mutable ret = 0
                while reader.Read() do 
                    ret <- reader.GetInt32(0) + 1
                conn.Close()
                ret
            with
                | ex -> raise ex
    
module ATP =
    let Remove (ct: IBaseSQLCommands) entity =
        let query, logs = QueryBuilder.BuildDeleteQuery entity
        ct.Execute query logs
    let GetPeoples (ct: IBaseSQLCommands) = 
        ct.Get (new Starikov.dbModels.People())
    let GetStudents (ct: IBaseSQLCommands) =
        //let ret = new List<Starikov.dbModels.Person>()
        seq { for v in ct.Get "student" do yield (Commands.Setter (new Starikov.dbModels.Student()) v) }
    let GetFIO (ct: IBaseSQLCommands) (man_id: obj) =
        let p = ct.GetWhere "people" (sprintf "(id_man='%O')" man_id) |> Seq.map (Commands.Setter (new Starikov.dbModels.People())) |> Seq.head
        sprintf "%s %c.%c." p.fam p.name.[0] p.otchestvo.[0]
    let GetGroups (ct: IBaseSQLCommands) =
        seq { for v in ct.Get "group" do yield (Commands.Setter (new Starikov.dbModels.Group()) v) }
        //ct.GetFromType <| typeof<Group> |> Option.get
    let GetOneGroup (ct: IBaseSQLCommands) (id_group: obj) =
        ct.GetWhere "group" (sprintf "(id_group='%O')" id_group) |> Seq.tryHead |> Option.map (Commands.Setter (new Starikov.dbModels.Group()))
    let GetGroupStudents (ct: IBaseSQLCommands) id_group =
        seq { for v in ct.GetWhere "student" (sprintf "(id_group='%d')" id_group) do yield (Commands.Setter (new Starikov.dbModels.Student()) v) }
    let GetAccounts (ct: IBaseSQLCommands) =
        ct.Get (new Starikov.dbModels.Account())
    let Log_People (context: IBaseSQLCommands) (target: LoginViewModel) =
        let L = target.Login.Split(' ')
        let People = 
            let qr = context.GetWhere "people" (sprintf "(fam='%s' AND name='%s' AND otchestvo='%s')" L.[0] L.[1] L.[2])
            if (Seq.length <| qr) >= 1 then (Seq.head <| qr) |> Some else printfn "Incorrect Login: {%s}!" target.Login; None
        People |> Option.map ( fun a -> Commands.Setter (new People()) a )
    let GetAccount (ct: IBaseSQLCommands) (man_id) =
        let person = (ct.GetWhere "people" (sprintf "(id_man='%s')" man_id)) |> Seq.head |> Commands.Setter (new People())
        let student = (ct.GetWhere "student" (sprintf "(id_man='%s')" man_id)) |> Seq.tryHead |> Option.map (Commands.Setter (new Student()))
        let retAcc = new AccountInfo(Person = person)
        if student.IsSome then
            retAcc.IsStudent <- true
            retAcc.Student <- student.Value
        retAcc
    let GetEventsInfos (ct: IBaseSQLCommands) =
        ct.Get (new Starikov.dbModels.EventInfo())
    let InsertAccount (ct: IBaseSQLCommands) (acc: Account) =
        let names, values = Commands.Getter <| acc |> Array.map (fun (n, v) -> (("`" + n + "`"), sprintf "'%O'" v)) |> Array.unzip
        let tableName = sprintf "%s" (((acc.GetType()).Name).ToLower())
        let fNames = names |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        let fValues = values |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        try
            let res = ct.Insert tableName (sprintf "(%s)" fNames) (sprintf "(%s)" fValues)
            printfn "Inserting account({%d}) result: %s" acc.id_man res
            true
        with
            | _ -> false
    let InsertEventInfo (ct: IBaseSQLCommands) (einfo: EventInfo) =
        let mutable fake = new DateTime()
        let names, values = einfo |> Commands.Getter |> Array.filter (fun (_, v) -> v |> isNull |> not) |> Array.map (fun (n, v) -> (("`" + n + "`"), sprintf "'%O'" v) ) |> Array.unzip
        let tableName = sprintf "%s" (((einfo.GetType()).Name).ToLower())
        let fNames = names.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        let fValues = values.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") "" |> sprintf "(%s)"
        try
            let res = ct.Insert tableName (sprintf "(%s)" fNames) fValues //((sprintf "( '%d" (ct.GetPK <| einfo.GetType())) + fValues.[4..] )
            printfn "Result: %s" res
            true
        with
            | _ -> false
    let InsertEvent (ct: IBaseSQLCommands) (event: Starikov.dbModels.Event) =
        let mutable fake = false
        let names, values = Commands.Getter <| event |> Array.map (fun (n, v) -> (("`" + n + "`"), (sprintf "'%O'" v))) |> Array.unzip
        let tableName = sprintf "%s" (((event.GetType()).Name).ToLower())
        let fNames = names.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        let fValues = values.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") "" |> sprintf "(%s)"
        try
            let res = ct.Insert tableName (sprintf "(%s)" fNames) fValues 
            printfn "Result: %s" res
            true
        with
            | _ -> false
    let InsertExtraEvent (ct: IBaseSQLCommands) (exEvent: Starikov.dbModels.ExtraEvent) =
        let names, values = Commands.Getter <| exEvent |> Array.map (fun (n, v) -> (("`" + n + "`"), (sprintf "'%O'" v))) |> Array.unzip
        let tableName = sprintf "%s" (((exEvent.GetType()).Name).ToLower())
        let fNames = names.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") ""
        let fValues = values.[1..] |> Array.fold (sprintf "%s, %s") "" |> Seq.tail |> Seq.fold (sprintf "%s%c") "" |> sprintf "(%s)"
        try
            let res = ct.Insert tableName (sprintf "(%s)" fNames) fValues 
            printfn "Result: %s" res
            true
        with
            | _ -> false
    let GetAnceteData (ct: IBaseSQLCommands) (man_id': obj) =
        let man_id = (man_id'.ToString()) |> int
        let NC target = if target = null then "" else target
        let abPerson = ct.GetFiltered (fun (p: People) -> p.id_man = man_id) |> Seq.head   //.GetWhere "people" (sprintf "(id_man='%s')" man_id) |> Seq.head |> Commands.Setter (new People())
        let abStudent = ct.GetFiltered (fun (s: Student) -> s.id_man = man_id) |> Seq.head  //.GetWhere "student" (sprintf "(id_man='%s')" man_id) |> Seq.head |> Commands.Setter (new Student())
        let abGroup = if abStudent.id_group.HasValue then (ct.GetFiltered (fun (g: dbModels.Group) -> g.id_group = abStudent.id_group.Value) |> Seq.head) else (new dbModels.Group()) 
        let abAncete = ct.GetFiltered (fun (ai: AnceteInfo) -> ai.id_man = man_id) |> Seq.tryHead |> function None -> new AnceteInfo() | Some(ai) -> ai
        let alter_lang = 
            if abAncete.id_language < 1 then "Выберите язык"
            else ct.GetFiltered (fun (lang: LanguageDictionary) -> abAncete.id_language = lang.id_language) |> Seq.map (fun lang -> lang.langName) |> Seq.head
        let mother, father = 
            match abAncete.student_mother, abAncete.student_father with
            | 0, 0 -> new People(), new People()
            | _, 0 -> ct.GetFiltered (fun (p: People) -> p.id_man = abAncete.student_mother) |> Seq.head, new People()
            | 0, _ -> new People(), ct.GetFiltered (fun (p: People) -> p.id_man = abAncete.student_father) |> Seq.head
            | _, _ -> ct.GetFiltered (fun (p: People) -> p.id_man = abAncete.student_mother || p.id_man = abAncete.student_father) 
                      |> fun parents -> parents |> Seq.filter (fun p -> p.id_man = abAncete.student_mother) |> Seq.head, parents |> Seq.filter (fun p -> p.id_man = abAncete.student_father) |> Seq.head
        //let childrens = ct.GetFiltered (new Childrens()) (fun cs -> cs.id_man = man_id) |> Seq.fold (fun state cs -> sprintf "%s%A\n" state cs.id_man_child) ""
        let ret = //new Anceta(lastname = abPerson.fam, name = abPerson.name, patron = abPerson.otchestvo, group = abGroup.name_group, birthdate = DateTime.Parse(abPerson.data_rojdeniya), grajdanstvo = abPerson.nacionalnost, voinskii_uchet = abPerson.nomer_vb, education = abPerson.chto_zakonchil, family_status = abPerson.semeinoe_polojenie, (*Childrens = ,*) pasport_serial = abPerson.serial_pasport, pasport_number = abPerson.number_pasport, pasport_getter = (sprintf "%s %s" abPerson.data_vidachi_pasporta abPerson.kem_vidan), (*pasport_code = ,*) inn = abPerson.INN, PFRF = abPerson.sv_vo_PFR, pIndex = abPerson.index_1, pRegion = abPerson.region_1, pCity = abPerson.gorod_1, (*pDistrict = ,*) pStreet = abPerson.ulica_1, pHome = abPerson.dom_1, pRoom = abPerson.kv_1, fIndex = abPerson.index_2, fRegion = abPerson.region_2, fCity = abPerson.gorod_2, (*fDistrict = ,*) fStreet = abPerson.ulica_2, fHome = abPerson.dom_2, fRoom = abPerson.kv_2, d_tel = abPerson.telefon_dom, m_tel = abPerson.telefon_sot, (*alter_lang = ,*) (*Bonuses = ,*) (*educationType = ,*) dealNumber = abStudent.number_kontrakta (*, dealStartDate = ,*) (*whoPay = ,*) (*pastSport = ,*) (*presantSport = ,*) (*futureSport = ,*) (*motherContact = ,*) (*fatherContact = ,*) )
            let a = new Anceta(lastname = abPerson.fam, name = abPerson.name, patron = abPerson.otchestvo)
            a.group <- NC abGroup.name_group
            a.birthdate <- NC abPerson.data_rojdeniya
            a.grajdanstvo <- NC abPerson.nacionalnost
            a.voinskii_uchet <- NC abPerson.nomer_vb
            a.education <- NC abPerson.chto_zakonchil
            a.exam_power <- NC abAncete.examenPower
            a.family_status <- NC abPerson.semeinoe_polojenie //
            a.Childrens <- NC abAncete.childrens
            a.pasport_serial <- NC abPerson.serial_pasport
            a.pasport_number <- NC abPerson.number_pasport
            a.pasport_date <- NC abPerson.data_vidachi_pasporta
            a.pasport_getter <- NC abPerson.kem_vidan
            a.pasport_code <- NC abAncete.pasport_code
            a.inn <- NC abPerson.INN
            a.PFRF <- NC abPerson.sv_vo_PFR
            a.pIndex <- NC abPerson.index_1
            a.pRegion <- NC abPerson.region_1
            a.pCity <- NC abPerson.gorod_1
            a.pDistrict <- NC abAncete.district_1
            a.pStreet <- NC abPerson.ulica_1
            a.pHome <- NC abPerson.dom_1
            a.pRoom <- NC abPerson.kv_1
            a.fIndex <- NC abPerson.index_2
            a.fRegion <- NC abPerson.region_2
            a.fCity <- NC abPerson.gorod_2
            a.fDistrict <- NC abAncete.district_2
            a.fStreet <- NC abPerson.ulica_2
            a.fHome <- NC abPerson.dom_2
            a.fRoom <- NC abPerson.kv_2
            a.d_tel <- NC abPerson.telefon_dom
            a.m_tel <- NC abPerson.telefon_sot
            a.alter_lang <- NC alter_lang
            a.Bonuses <- NC abAncete.benefits
            a.educationType <- NC abAncete.educ_type
            a.dealNumber <- NC abStudent.number_kontrakta
            a.dealStartDate <- NC abAncete.kontract_startday
            a.whoPay <- NC abAncete.who_pays_kontract
            a.pastSport <- NC abAncete.pastSport
            a.presantSport <- NC abAncete.presentSport
            a.futureSport <- NC abAncete.futureSport
            a.MotherFIO <- sprintf "%s %s %s" mother.fam mother.name mother.otchestvo |> fun s -> if System.String.IsNullOrWhiteSpace(s) || s.Length <= 2 then "" else s
            a.MotherRangAndWork <- sprintf "%s, %s" mother.doljnost mother.mesto_raboti |> fun s -> if System.String.IsNullOrWhiteSpace(s) || s.Length <= 2 then "" else s
            a.MotherWorkerPhone <- mother.telefon_rabochii
            a.FatherFIO <- sprintf "%s %s %s" father.fam father.name father.otchestvo |> fun s -> if System.String.IsNullOrWhiteSpace(s) || s.Length <= 2 then "" else s
            a.FatherRangAndWork <- sprintf "%s, %s" father.doljnost father.mesto_raboti |> fun s -> if System.String.IsNullOrWhiteSpace(s) || s.Length <= 2 then "" else s
            a.FatherWorkerPhone <- father.telefon_rabochii
            a
        ret
    let SetAnceteData (ct: IBaseSQLCommands) (man_id': obj) (a: Anceta) =
        let man_id = (man_id'.ToString()) |> int
        let (!=) f s = (Commands.Equal f s) |> not
        let abP', abP = ct.GetFiltered (fun (p: People) -> p.id_man = man_id) |> Seq.head |> Commands.CopyObj
        let abS', abS = ct.GetFiltered (fun (s: Student) -> s.id_man = man_id) |> Seq.head |> Commands.CopyObj
        let abG', abG = (if abS.id_group.HasValue then (ct.GetFiltered (fun (g: dbModels.Group) -> g.id_group = abS.id_group.Value) |> Seq.head) else (new dbModels.Group()) ) |> Commands.CopyObj
        let mutable hasAncete = true
        let abA', abA = (ct.GetFiltered (fun (ai: AnceteInfo) -> ai.id_man = man_id) |> Seq.tryHead |> function None -> hasAncete <- false; new AnceteInfo(id_man = man_id) | Some(ai) -> ai) |> Commands.CopyObj
        // let mct = this :> IMyDBContext
        let gnames = ct |> GetGroups |> Seq.map (fun g -> g.name_group) |> Seq.toList
        // let langs = ct.Get (new LanguageDictionary()) |> Seq.map (fun l -> l.langName) |> Seq.toList
        let da = GetAnceteData ct man_id
        let now = System.DateTime.Now.ToString("MM-dd-yyyy")
        let check = Commands.SC (man_id'.ToString())
        printfn "Has ancete: %b" hasAncete
        if check (a.lastname, da.lastname) then abP.fam <- a.lastname
        if check (a.name, da.name) then abP.name <- a.name
        if check (a.patron, da.patron) then abP.otchestvo <- a.patron
        if check (a.group, da.group) then abS.id_group <- a.group |> (fun gname ->
                                                                        let cname = Checker.Choose gname gnames
                                                                        let f = ct |> GetGroups |> Seq.filter (fun t -> t.name_group = cname)
                                                                        let ret = f |> Seq.tryHead |> Option.map (fun h -> h.id_group)
                                                                        if ret.IsSome then (new System.Nullable<int>(ret.Value)) else abS.id_group
                                                                     )
        if check (a.birthdate, da.birthdate) then abP.data_rojdeniya <- a.birthdate
        if check (a.grajdanstvo, da.grajdanstvo) then abP.nacionalnost <- a.grajdanstvo
        if check (a.voinskii_uchet, da.voinskii_uchet) then abP.nomer_vb <- a.voinskii_uchet
        if check (a.education, da.education) then abP.chto_zakonchil <- a.education
        if check (a.exam_power, da.exam_power) then abA.examenPower <- a.exam_power
        if check (a.family_status, da.family_status) then abP.semeinoe_polojenie <- a.family_status
        if check (a.Childrens, da.Childrens) then abA.childrens <- a.Childrens
        if check (a.pasport_serial, da.pasport_serial) then abP.serial_pasport <- a.pasport_serial
        if check (a.pasport_number, da.pasport_number) then abP.number_pasport <- a.pasport_number
        if check (a.pasport_date, da.pasport_date) then abP.data_vidachi_pasporta <- a.pasport_date
        if check (a.pasport_getter, da.pasport_getter) then abP.kem_vidan <- a.pasport_getter
        if check (a.pasport_code, da.pasport_code) then abA.pasport_code <- a.pasport_code
        if check (a.inn, da.inn) then abP.INN <- a.inn
        if check (a.PFRF, da.PFRF) then abP.sv_vo_PFR <- a.PFRF
        if check (a.pIndex, da.pIndex) then abP.index_1 <- a.pIndex
        if check (a.pRegion, da.pRegion) then abP.region_1 <- a.pRegion
        if check (a.pCity, da.pCity) then abP.gorod_1 <- a.pCity
        if check (a.pDistrict, da.pDistrict) then abA.district_1 <- a.pDistrict
        if check (a.pStreet, da.pStreet) then abP.ulica_1 <- a.pStreet
        if check (a.pHome, da.pHome) then abP.dom_1 <- a.pHome
        if check (a.pRoom, da.pRoom) then abP.kv_1 <- a.pRoom
        if check (a.fIndex, da.fIndex) then abP.index_2 <- a.fIndex
        if check (a.fRegion, da.fRegion) then abP.region_2 <- a.fRegion
        if check (a.fCity, da.fCity) then abP.gorod_2 <- a.fCity
        if check (a.fDistrict, da.fDistrict) then abA.district_2 <- a.fDistrict
        if check (a.fStreet, da.fStreet) then abP.ulica_2 <- a.fStreet
        if check (a.fHome, da.fHome) then abP.dom_2 <- a.fHome
        if check (a.fRoom, da.fRoom) then abP.kv_2 <- a.fRoom
        if check (a.d_tel, da.d_tel) then abP.telefon_dom <- a.d_tel
        if check (a.m_tel, da.m_tel) then abP.telefon_sot <- a.m_tel
        if check (a.alter_lang, da.alter_lang) then abA.id_language <- a.alter_lang |> fun lang_name ->
                            let lang = ct.GetFiltered (fun (l: LanguageDictionary) -> l.langName = lang_name) |> Seq.tryHead
                            if lang.IsSome then lang.Value.id_language else abA'.id_language
        if check (a.Bonuses, da.Bonuses) then abA.benefits <- a.Bonuses
        if check (a.educationType, da.educationType) then abA.educ_type <- a.educationType
        if check (a.dealNumber, da.dealNumber) then abS.number_kontrakta <- a.dealNumber
        if check (a.dealStartDate, da.dealStartDate) then abA.kontract_startday <- a.dealStartDate
        if check (a.whoPay, da.whoPay) then abA.who_pays_kontract <- a.whoPay
        if check (a.pastSport, da.pastSport) then abA.pastSport <- a.pastSport
        if check (a.presantSport, da.presantSport) then abA.presentSport <- a.presantSport
        if check (a.futureSport, da.futureSport) then abA.futureSport <- a.futureSport
        if check (a.MotherFIO, da.MotherFIO) || check (a.MotherRangAndWork, da.MotherRangAndWork) || check (a.MotherWorkerPhone, da.MotherWorkerPhone) then abA.student_mother <- (a.MotherFIO, a.MotherRangAndWork, a.MotherWorkerPhone) |> fun (n, w, t) ->
            let fio = if System.String.IsNullOrEmpty(n) then [|""; ""; ""|] else n.Split ([|' '|])
            let dm = if System.String.IsNullOrEmpty(w) then [|""; ""|] else w.Split ([| ','; ';'; '.' |])
            let oldData = ct.GetFiltered (fun (p: People) -> p.id_man = abA'.student_mother) |> Seq.tryHead
            if oldData.IsSome then
                let data', data = oldData.Value |> Commands.CopyObj
                data.fam <- fio.[0]; data.name <- fio.[1]; data.otchestvo <- fio.[2]
                data.doljnost <- dm.[0]
                let mr = dm.[1..] |> Array.fold (fun state e -> sprintf "%s%s" state e) ""
                data.mesto_raboti <- mr
                data.telefon_rabochii <- t
                if data' != data then
                    data.data_change <- now
                    let mquery, mlogs = QueryBuilder.BuildUpdateQuery data' data
                    ct.Execute mquery mlogs |> ignore
                data'.id_man
            else
                let data = new People()
                data.fam <- fio.[0]; data.name <- fio.[1]; data.otchestvo <- fio.[2]
                data.doljnost <- dm.[0]
                let mr = dm.[1..] |> Array.fold (fun state e -> sprintf "%s%s" state e) ""
                data.mesto_raboti <- mr
                data.telefon_rabochii <- t
                data.data_change <- now
                let fp, sp, tp = QueryBuilder.BuildInsertQueryWithAI data
                printfn "Befor insert..."
                ct.Insert fp sp tp |> printfn "Insert result: {%s}"
                printfn "After insert..."    // Опасно так делать, при ошибках будет больно!
                let inserted = ct.GetFiltered (fun (p: People) -> p.fam = data.fam && p.name = data.name && p.otchestvo = data.otchestvo && p.doljnost = data.doljnost && p.mesto_raboti = data.mesto_raboti && p.telefon_rabochii = data.telefon_rabochii) |> Seq.tryHead
                if inserted.IsSome then inserted.Value.id_man |> fun e -> printfn "Out: {%A}" e; e else printfn "Mother not created!"; 0
        if check (a.FatherFIO, da.FatherFIO) || check (a.FatherRangAndWork, da.FatherRangAndWork) || check (a.FatherWorkerPhone, da.FatherWorkerPhone) then abA.student_father <- (a.FatherFIO, a.FatherRangAndWork, a.FatherWorkerPhone) |> fun (n, w, t) ->
            let fio = if System.String.IsNullOrEmpty(n) then [|""; ""; ""|] else n.Split ([|' '|])
            let dm = if System.String.IsNullOrEmpty(w) then [|""; ""|] else w.Split ([| ','; ';'; '.' |])
            let oldData = ct.GetFiltered (fun (p: People) -> p.id_man = abA'.student_father) |> Seq.tryHead
            if oldData.IsSome then
                let data', data = oldData.Value |> Commands.CopyObj
                data.fam <- fio.[0]; data.name <- fio.[1]; data.otchestvo <- fio.[2]
                data.doljnost <- dm.[0]
                let mr = dm.[1..] |> Array.fold (fun state e -> sprintf "%s%s" state e) ""
                data.mesto_raboti <- mr
                data.telefon_rabochii <- t
                if data' != data then
                    data.data_change <- now
                    let mquery, mlogs = QueryBuilder.BuildUpdateQuery data' data
                    ct.Execute mquery mlogs |> ignore
                data'.id_man
            else
                let data = new People()
                data.fam <- fio.[0]; data.name <- fio.[1]; data.otchestvo <- fio.[2]
                data.doljnost <- dm.[0]
                let mr = dm.[1..] |> Array.fold (fun state e -> sprintf "%s%s" state e) ""
                data.mesto_raboti <- mr
                data.telefon_rabochii <- t
                data.data_change <- now
                let fp, sp, tp = QueryBuilder.BuildInsertQueryWithAI data
                ct.Insert fp sp tp |> ignore    // Опасно так делать, при ошибках будет больно!
                let inserted = ct.GetFiltered (fun (p: People) -> p.fam = data.fam && p.name = data.name && p.otchestvo = data.otchestvo && p.doljnost = data.doljnost && p.mesto_raboti = data.mesto_raboti && p.telefon_rabochii = data.telefon_rabochii) |> Seq.tryHead
                if inserted.IsSome then inserted.Value.id_man |> fun e -> printfn "Out: {%A}" e; e else printfn "Father not created!"; 0
        //Commands.SC man_id (anceta.birthdate.ToString(), defaultAnceta.birthdate.ToString(), &abPerson.data_rojdeniya)
        try
            if abP' != abP then
                printfn "Start update people..."
                //abP |> Commands.EntityToString |> printfn "New people: %s"
                abP.data_change <- now
                abP.data_zapoln_anketi <- now
                let pquery, plogs = QueryBuilder.BuildUpdateQuery abP' abP
                //printfn "QUERY: {%s}\tLOG: {%s}" pquery plogs
                let pres = ct.Execute pquery plogs
                printfn "People update is good! Result: \"%s\"" pres
            if abS' != abS then
                printfn "Start update student..."
                //abS |> Commands.EntityToString |> printfn "New student: %s"
                abS.data_change <- now
                let squery, slogs = QueryBuilder.BuildUpdateQuery abS' abS
                //printfn "QUERY: {%s}\tLOG: {%s}" pquery plogs
                let sres = ct.Execute squery slogs
                printfn "Student update is good! Result: \"%s\"" sres
            if abA' != abA then
                printfn "Start update ancete..."
                abA.date_of_change <- now
                let ares = 
                    if hasAncete then
                        //abA |> Commands.EntityToString |> printfn "New ancete: %s"
                        let aquery, alogs = QueryBuilder.BuildUpdateQuery abA' abA
                        //printfn "QUERY: {%s}\tLOG: {%s}" pquery plogs
                        ct.Execute aquery alogs
                    else
                        //abA |> Commands.EntityToString |> printfn "New ancete: %s"
                        let fp, sp, tp = QueryBuilder.BuildInsertQueryWithoutAI abA
                        ct.Insert fp sp tp
                printfn "AnceteInfo update is good! Result: \"%s\"" ares
            true
        with
            | _ -> printfn "Ошибка добавления анкеты"; false