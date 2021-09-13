module Kr : sig
  type schedule = Quarterly of [ `Q1 | `Q2 | `Q3 | `Q4 ] * int | Rolling
  [@@deriving yojson]

  type status =
    | Scheduled of schedule
    | Unscheduled
    | No_status
    | Active
    | Unfunded
    | Blocked
  [@@deriving yojson]

  val status_of_string : string -> status

  type t = {
    title : string;
    owner : string option;
    ident : string;
    status : status;
  }
  [@@deriving yojson]

  val v : status:status -> string -> t
end

module Objective : sig
  type t = { title : string; krs : Kr.t list } [@@deriving yojson]

  val of_block : Omd.doc -> t
end

module Project : sig
  type t = { project : string; objectives : Objective.t list }
  [@@deriving yojson]

  val of_block : Omd.doc -> t
end

type t = Project.t list [@@deriving yojson]

val of_block : Omd.doc -> t
val from_file : string -> t
