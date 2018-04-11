namespace Starikov

open System
open Starikov.dbModels
type IBaseSQLCommands =
    abstract member Execute : string -> string -> string
    abstract member Get : string -> seq<obj []>
    abstract member Get : 'T -> seq<'T>
    abstract member GetFromType : System.Type -> seq<'T> option
    abstract member GetWhere : string -> string -> seq<obj []>
    abstract member GetFiltered : ('T -> bool) -> seq<'T>
    abstract member Insert : string -> string -> string -> string
    abstract member Update : string -> string -> string -> string
    abstract member GetPK : System.Type -> int
type IMyDBContext =
    abstract member Remove : 'T -> string
    abstract member GetPeoples : seq<People>
    abstract member GetStudents : seq<Student>
    abstract member GetFIO : obj -> string
    abstract member GetGroups : seq<Group>
    abstract member GetOneGroup : obj -> Group option
    abstract member GetGroupStudents : int -> seq<Student>
    abstract member GetAccounts : seq<Starikov.dbModels.Account>
    abstract member Log_People : LoginViewModel -> People option
    abstract member GetAccount : string -> AccountInfo
    abstract member GetEventsInfos : seq<EventInfo>
    abstract member InsertAccount : Account -> bool
    abstract member InsertEventInfo : EventInfo -> bool
    abstract member InsertEvent : Starikov.dbModels.Event -> bool
    abstract member InsertExtraEvent : Starikov.dbModels.ExtraEvent -> bool
    abstract member GetAnceteData : obj -> Anceta
    abstract member SetAnceteData : obj -> Anceta -> bool
type IMessager =
    abstract member SendMessage : string -> string -> seq<string * string> -> unit
type IAccountRegistrationHelper =
    abstract member GetCuratorsKeyWord : string
    abstract member GetAdminsKeyWord : string    