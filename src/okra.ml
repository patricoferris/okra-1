(*
 * Copyright (c) 2021 Magnus Skjegstad
 * Copyright (c) 2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Omd

let hashtbl_keys ht =
  List.sort_uniq String.compare (List.of_seq (Hashtbl.to_seq_keys ht))

exception No_time_found of string
exception Multiple_time_entries of string
exception KR_title_id_mismatch of string

(* Type for sanitized post-ast version *)
type t = {
  counter : int;
  project : string;
  objective : string;
  kr_title : string;
  kr_id : string;
  time_entries : string list;
  time_per_engineer : (string, float) Hashtbl.t;
  work : string list;
}

let compare a b =
  if String.compare a.project b.project = 0 then
    (* compare on project first *)
    if String.compare a.objective b.objective = 0 then
      (* then obj if proj equal *)
      (* Check if KR IDs are the same --if one of the KR IDs are
         blank, compare on title instead *)
      let compare_kr_id =
        if
          a.kr_id = ""
          || b.kr_id = ""
          || a.kr_id = "NEW KR"
          || b.kr_id = "NEW KR"
          || a.kr_id = "NEW OKR"
          || b.kr_id = "NEW OKR"
        then String.compare a.kr_title b.kr_title
        else String.compare a.kr_id b.kr_id
      in
      (* If KRs match, check counter *)
      if compare_kr_id = 0 then compare a.counter b.counter else compare_kr_id
    else String.compare a.objective b.objective
  else String.compare a.project b.project

module Weekly = struct
  (* Types for parsing the AST *)
  type elt =
    | O of string (* Objective name, without lead *)
    | Proj of string (* Project name / pillar *)
    | KR of string (* Full name of KR, with ID *)
    | KR_id of string (* ID of KR *)
    | KR_title of string (* Title without ID, tech lead *)
    | Work of string list (* List of work items *)
    | Time of string (* Time entry *)
    | Counter of int
  (* Increasing counter to be able to sort multiple entries by time *)

  type t = elt list list
  type table = (string, t) Hashtbl.t

  let has_proj okr_list proj =
    let m =
      List.find_all
        (fun f ->
          match f with
          | Proj s ->
              Printf.printf "%s=%s\n" s proj;
              String.compare s proj = 0
          | _ -> false)
        okr_list
    in
    List.length m > 0

  let filter okr_ll proj = List.filter (fun f -> has_proj f proj) okr_ll

  let pp ppf okr_list =
    let pf fmt = Fmt.pf ppf fmt in
    (* Just print O/KR from first item *)
    List.iter
      (fun el ->
        match el with
        | Proj s -> pf "\n# %s\n" s
        | O s -> pf "\n## %s\n" s
        | KR s -> pf "\n- %s\n" s
        | _ -> ())
      (List.hd okr_list);
    (* Just find time *)
    let ht_t = Hashtbl.create 7 in
    let _ =
      List.map
        (fun elements ->
          List.iter
            (fun el ->
              match el with
              | Time t_ ->
                  pf "    - + %s" t_;
                  (* todo this shouldn't happen when printing, but earlier *)
                  (* split on @, then extract first word and any float after *)
                  let t_split = Str.split (Str.regexp "@+") t_ in
                  List.iter
                    (fun s ->
                      match
                        Str.string_match
                          (Str.regexp "^\\([a-zA-Z0-9-]+\\)[^0-9]+\\([0-9.]+\\)")
                          s 0
                      with
                      | false -> ()
                      | true ->
                          let user = Str.matched_group 1 s in
                          (* todo: let this conversion raise an
                             exception, would be nice to exit more
                             cleanly, but it should be fatal *)
                          let days = Float.of_string (Str.matched_group 2 s) in
                          (*pf "-  + @%s (%.1f days)\n" user days;*)
                          Hashtbl.add ht_t user days)
                    t_split
              | _ -> ())
            elements)
        okr_list
    in
    (* Sum and print time *)
    pf "    - = ";
    List.iter
      (fun key ->
        let sum =
          List.fold_left
            (fun a b -> Float.add a b)
            0.0
            (Hashtbl.find_all ht_t key)
        in
        pf "@%s (%.2f days) " key sum)
      (hashtbl_keys ht_t);
    pf "\n";
    (* Just print work *)
    let _ =
      List.map
        (fun elements ->
          List.iter
            (fun el ->
              match el with
              | Work w -> List.iter (fun w_ -> pf "    - %s" w_) w
              | _ -> ())
            elements)
        okr_list
    in
    ()
end

open Weekly

let okr_re = Str.regexp "\\(.+\\) (\\([a-zA-Z]+[0-9]+\\))$"
(* Header: This is a KR (KR12) *)

let obj_re = Str.regexp "\\(.+\\) (\\([a-zA-Z ]+\\))$"
(* Header: This is an objective (Tech lead name) *)

let new_okr_re = obj_re

let check_valid_time_block = function
  | [ Paragraph (_, Text (_, s)) ] -> String.get (String.trim s) 0 = '@'
  | _ -> false

let is_suffix suffix s =
  String.length s >= String.length suffix
  &&
  let suffix = String.uppercase_ascii suffix in
  let s = String.uppercase_ascii s in
  String.equal suffix
    (String.sub s
       (String.length s - String.length suffix)
       (String.length suffix))

let parse_okr_title s =
  (* todo: could match on ??) too? *)
  if is_suffix "(new kr)" s || is_suffix "(new okr)" s then
    match Str.string_match new_okr_re s 0 with
    | false -> None
    | true -> Some (String.trim (Str.matched_group 1 s), "new KR")
  else
    match Str.string_match okr_re s 0 with
    | false -> None
    | true ->
        Some
          ( String.trim (Str.matched_group 1 s),
            String.trim (Str.matched_group 2 s) )

let pp ppf okr =
  let pf fmt = Fmt.pf ppf fmt in
  let pp ppf = function
    | Proj s -> pf "P: %s" s
    | O s -> pf "O: %s" s
    | KR s -> pf "KR: %s" s
    | KR_id s -> pf "KR id: %s" s
    | KR_title s -> pf "KR title: %s" s
    | Work w ->
        let pp ppf e = Fmt.pf ppf "W: %s" e in
        Fmt.list ~sep:(Fmt.unit ", ") pp ppf w
    | Time _ -> pf "Time: <not shown>"
    | Counter c -> pf "Cnt: %d" c
  in
  Fmt.list ~sep:(Fmt.unit ", ") pp ppf okr

let store_result store okr_list =
  let key1 =
    let f =
      List.find_opt
        (fun xs -> match xs with KR_title _ -> true | _ -> false)
        okr_list
    in
    match f with Some (KR_title t) -> t | _ -> "Unknown"
  in
  let key = Printf.sprintf "%s" (String.uppercase_ascii key1) in
  let has_time =
    match
      List.find_opt
        (fun xs -> match xs with Time _ -> true | _ -> false)
        okr_list
    with
    | None -> false
    | Some _ -> true
  in
  match has_time with
  | false -> Fmt.pr "WARNING: Ignored %a\n" pp okr_list
  | true -> (
      match Hashtbl.find_opt store key with
      | None -> Hashtbl.add store key [ okr_list ]
      | Some x -> Hashtbl.replace store key (x @ [ okr_list ]))

let rec inline = function
  | Concat (_, xs) -> List.concat (List.map inline xs)
  | Text (_, s) -> [ s ]
  | Emph (_, s) -> "*" :: inline s @ [ "*" ]
  | Strong (_, s) -> "**" :: inline s @ [ "**" ]
  | Code (_, s) -> [ "`"; s; "`" ]
  | Hard_break _ -> [ "\n\n" ]
  | Soft_break _ -> [ "\n" ]
  | Link (_, { label; destination; _ }) ->
      "[" :: inline label @ [ "]("; destination; ")" ]
  | Html _ -> [ "**html-ignored**" ]
  | Image _ -> [ "**img ignored**" ]

let rec block = function
  | Paragraph (_, x) -> inline x @ [ "\n" ]
  | List (_, _, _, bls) ->
      List.concat
        (List.map
           (fun xs -> "        - " :: List.concat (List.map block xs))
           bls)
  | Blockquote (_, x) -> "> " :: List.concat (List.map block x)
  | Thematic_break _ -> [ "*thematic-break-ignored*" ]
  | Heading (_, level, text) -> String.make level '#' :: inline text @ [ "\n" ]
  | Code_block (_, info, _) -> [ "```"; info; "```" ]
  | Html_block _ -> [ "*html-ignored*" ]
  | Definition_list _ -> [ "*def-list-ignored*" ]

let block_okr = function
  | Paragraph (_, x) -> (
      let okr_title = String.trim (String.concat "" (inline x)) in
      match parse_okr_title okr_title with
      | None -> [ KR okr_title; KR_title okr_title ]
      | Some (title, id) -> [ KR okr_title; KR_title title; KR_id id ])
  | List (_, _, _, bls) ->
      if List.length (List.filter check_valid_time_block bls) > 1 then
        (* This is fatal, as we can miss tracked time if this occurs
            -- time should always be summarised in first entry *)
        raise (Multiple_time_entries "Multiple time entries found")
      else ();
      let tb = List.hd bls in
      (* Assume first block is time if present, and ... *)
      if check_valid_time_block tb then
        (* verify that this is true *)
        let time_s =
          String.concat "" (List.map (fun xs -> String.concat "" (block xs)) tb)
        in
        let work_items =
          Work
            (List.map
               (fun xs -> String.concat "" (List.concat (List.map block xs)))
               (List.tl bls))
        in
        [ Time time_s; work_items ]
      else []
  | _ -> []

let strip_obj_lead s =
  match Str.string_match obj_re (String.trim s) 0 with
  | false -> s
  | true -> Str.matched_group 1 s

type state = {
  mutable current_o : string;
  mutable current_proj : string;
  mutable current_counter : int;
}

let init () =
  { current_o = "None"; current_proj = "Unknown"; current_counter = 0 }

let process_okr_block t ht hd tl =
  (* peek at each block in list, consume if match - otherwise return
     full list for regular processing*)
  match hd with
  | Heading (_, _, il) ->
      let title =
        match il with
        (* Display header with level, strip lead from objectives if present *)
        | Text (_, s) -> strip_obj_lead s
        | _ -> "None"
      in
      (* remember last object title seen - works if Os have not been
         renamed in a set of files *)
      t.current_o <- title;
      tl
  | List (_, _, _, bls) ->
      let _ =
        List.map
          (fun xs ->
            let okr_list = List.concat (List.map block_okr xs) in
            (*if List.length okr_list = 3 then
                 okr_list
               else (* return empty list if it doesn't include Time/Work/OKR *)
                 okr_list) bls in*)
            store_result ht
              ([ Proj t.current_proj; O t.current_o; Counter t.current_counter ]
              @ okr_list))
          bls
      in
      t.current_counter <- t.current_counter + 1;
      tl
  | _ -> tl

let process_entry t ht hd tl =
  (* Find project level headers *)
  (match hd with
  | Heading (_, level, il) -> (
      match (level, il) with
      (* Display header with level, strip lead from objectives if present *)
      | 1, Text (_, s) -> t.current_proj <- s
      | _, _ -> ())
  | _ -> ());
  process_okr_block t ht hd tl

let rec process t ht ast =
  match ast with
  | [ hd ] -> process t ht (process_entry t ht hd [])
  | hd :: tl -> process t ht (process_entry t ht hd tl)
  | [] -> ()

let process ast =
  let state = init () in
  let store = Hashtbl.create 100 in
  process state store ast;
  store

let of_weekly okr_list =
  (* This function expects a list of entries for the same KR, typically
     corresponding to a set of weekly reports. Each list item will consist of
     a list of okr_t items, which provides time, work items etc for this entry.

     This function will aggregate all entries for the same KR in an
     okr_entry record for easier processing later.
  *)
  let okr_proj = ref "" in
  let okr_obj = ref "" in
  let okr_kr_title = ref "" in
  let okr_kr_id = ref "" in
  let okr_counter = ref 0 in

  (* Assume each item in list has the same O/KR/Proj, so just parse
     the first one *)
  (* todo we could sanity check here by verifying that every entry has
     the same KR/O *)
  List.iter
    (fun el ->
      match el with
      | Proj s -> okr_proj := s
      | O s -> okr_obj := s
      | KR_title s -> okr_kr_title := s
      | KR_id s -> okr_kr_id := String.uppercase_ascii s
      | Counter x -> okr_counter := x
      | _ -> ())
    (List.hd okr_list);

  (* Find all the time records and store in hashtbl keyed by engineer
     + original *)
  let okr_time_entries = ref [] in
  let ht_t = Hashtbl.create 7 in
  List.iter
    (fun elements ->
      List.iter
        (fun el ->
          match el with
          | Time t_ ->
              (* Store the string entry to be able to check correctness later *)
              okr_time_entries := !okr_time_entries @ [ t_ ];
              (* split on @, then extract first word and any float after *)
              let t_split = Str.split (Str.regexp "@+") t_ in
              List.iter
                (fun s ->
                  match
                    Str.string_match
                      (Str.regexp "^\\([a-zA-Z0-9-]+\\)[^0-9]+\\([0-9.]+\\)")
                      s 0
                  with
                  | false -> ()
                  | true ->
                      let user = Str.matched_group 1 s in
                      (* todo: let this conversion raise an exception,
                         would be nice to exit more cleanly, but it
                         should be fatal *)
                      let days = Float.of_string (Str.matched_group 2 s) in
                      Hashtbl.add ht_t user days)
                t_split
          | _ -> ())
        elements)
    okr_list;

  (* Sum time per engineer *)
  let time_per_engineer = Hashtbl.create 7 in
  List.iter
    (fun key ->
      let sum =
        List.fold_left
          (fun a b -> Float.add a b)
          0.0
          (Hashtbl.find_all ht_t key)
      in
      Hashtbl.replace time_per_engineer key sum)
    (hashtbl_keys ht_t);

  (* Add work items in order, concat all the lists *)
  let work =
    List.concat
      (List.map
         (fun elements ->
           List.concat
             (List.map
                (fun el -> match el with Work w -> w | _ -> [])
                elements))
         okr_list)
  in

  (* Construct final entry *)
  {
    counter = !okr_counter;
    project = !okr_proj;
    objective = !okr_obj;
    kr_title = !okr_kr_title;
    kr_id = !okr_kr_id;
    time_entries = !okr_time_entries;
    time_per_engineer;
    work;
  }

module Unused = struct
  let _tim_re = Str.regexp "@\\([a-zA-Z0-9]+\\) (\\([0-9.]+\\) days?)$"
  (* @[github-handle] ([float] day/days) *)

  let _tim_split_re = Str.regexp " ?, ?"
  (* Split on , surrounded by zero or more whitespace *)

  let _is_prefix prefix s =
    String.length s >= String.length prefix
    &&
    let prefix = String.uppercase_ascii prefix in
    let s = String.uppercase_ascii s in
    String.equal prefix (String.sub s 0 (String.length prefix))

  let _get_proj_list store =
    (* iterate over ht *)
    let keys = hashtbl_keys store in
    List.sort_uniq String.compare
      (List.concat
         (List.map
            (fun key ->
              let okr_list = List.concat (Hashtbl.find store key) in
              List.concat
                (List.map
                   (fun el -> match el with Proj s -> [ s ] | _ -> [])
                   okr_list))
            keys))

  let _parse_obj s =
    match Str.string_match obj_re s 0 with
    | false -> None
    | true -> Some (Str.matched_group 1 s, Str.matched_group 2 s)
end
