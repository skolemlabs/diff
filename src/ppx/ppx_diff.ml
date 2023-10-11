open Ppxlib
open Ast_builder.Default

module Utils = struct
  let constructor_name_of_field ~field_name ~name =
    match name with
    | "t" -> String.capitalize_ascii field_name
    | _ -> String.capitalize_ascii name ^ "_" ^ field_name

  let getter_name ~name =
    match name with "t" -> "getter" | _ -> name ^ "_getter"

  let setter_name ~name =
    match name with "t" -> "getter" | _ -> name ^ "_setter"

  let field_lident ~loc = { loc; txt = Longident.parse "Diff.field" }
  let add_lident ~loc = { loc; txt = Longident.parse "Diff.add" }
  let unit_lident ~loc = { loc; txt = Lident "()" }
  let option_lident ~loc = { loc; txt = Lident "option" }
  let some_lident ~loc = { loc; txt = Lident "Some" }
  let none_lident ~loc = { loc; txt = Lident "None" }
  let getter_lident ~loc = { loc; txt = Longident.parse "Diff.getter" }
  let setter_lident ~loc = { loc; txt = Longident.parse "Diff.setter" }
end

module Impl = struct
  let generate_pre_attr ~loc =
    pstr_attribute ~loc
      (attribute ~loc
         ~name:{ txt = "ocaml.warning"; loc }
         ~payload:
           (PStr
              [
                pstr_eval ~loc
                  (pexp_constant ~loc (Pconst_string ("-23-39", loc, None)))
                  [];
              ]))

  let generate_post_attr ~loc =
    pstr_attribute ~loc
      (attribute ~loc
         ~name:{ txt = "ocaml.warning"; loc }
         ~payload:
           (PStr
              [
                pstr_eval ~loc
                  (pexp_constant ~loc (Pconst_string ("+23+39", loc, None)))
                  [];
              ]))

  let generate_field
      ~field:
        { pld_name = { txt = field_name; _ }; pld_loc = loc; pld_type = typ; _ }
      ~name ~ct =
    let txt = Utils.constructor_name_of_field ~field_name ~name in
    extension_constructor ~loc ~name:{ txt; loc }
      ~kind:
        (Pext_decl
           ( [],
             Pcstr_tuple [],
             Some (ptyp_constr ~loc (Utils.field_lident ~loc) [ ct; typ ]) ))

  let generate_fields ~fields ~name ~loc ~ct =
    pstr_typext ~loc
      (type_extension ~loc ~path:(Utils.field_lident ~loc)
         ~params:
           (List.init 2 (fun _ -> (ptyp_any ~loc, (NoVariance, NoInjectivity))))
         ~constructors:
           (List.map (fun field -> generate_field ~name ~field ~ct) fields)
         ~private_:Public)

  let generate_getter_case
      ~field:{ pld_name = { txt = field_name; _ }; pld_loc = loc; _ } ~name =
    case
      ~lhs:
        (ppat_construct ~loc
           {
             txt = Lident (Utils.constructor_name_of_field ~field_name ~name);
             loc;
           }
           None)
      ~guard:None
      ~rhs:
        (pexp_construct ~loc (Utils.some_lident ~loc)
           (Some
              (pexp_field ~loc
                 (pexp_ident ~loc { txt = Lident name; loc })
                 { txt = Lident field_name; loc })))

  let generate_getter_cases ~fields ~loc ~name =
    List.rev
      (case ~lhs:(ppat_any ~loc) ~guard:None
         ~rhs:(pexp_construct ~loc (Utils.none_lident ~loc) None)
      :: List.rev_map (fun field -> generate_getter_case ~field ~name) fields)

  let generate_getter ~fields ~name ~loc =
    value_binding ~loc
      ~pat:
        (ppat_constraint ~loc
           (ppat_var ~loc { txt = Utils.getter_name ~name; loc })
           (ptyp_constr ~loc (Utils.getter_lident ~loc) []))
      ~expr:
        (pexp_record ~loc
           [
             ( { txt = Lident "f"; loc },
               pexp_newtype ~loc { txt = "a"; loc }
                 (pexp_newtype ~loc { txt = "b"; loc }
                    (pexp_fun ~loc Nolabel None
                       (ppat_constraint ~loc
                          (ppat_var ~loc { txt = name; loc })
                          (ptyp_constr ~loc { txt = Lident "a"; loc } []))
                       (pexp_fun ~loc Nolabel None
                          (ppat_constraint ~loc
                             (ppat_var ~loc { txt = "field"; loc })
                             (ptyp_constr ~loc (Utils.field_lident ~loc)
                                [
                                  ptyp_constr ~loc { txt = Lident "a"; loc } [];
                                  ptyp_constr ~loc { txt = Lident "b"; loc } [];
                                ]))
                          (pexp_constraint ~loc
                             (pexp_match ~loc
                                (pexp_ident ~loc { txt = Lident "field"; loc })
                                (generate_getter_cases ~loc ~fields ~name))
                             (ptyp_constr ~loc (Utils.option_lident ~loc)
                                [
                                  ptyp_constr ~loc { txt = Lident "b"; loc } [];
                                ]))))) );
           ]
           None)

  let generate_setter_case
      ~field:{ pld_name = { txt = field_name; _ }; pld_loc = loc; _ } ~name =
    case
      ~lhs:
        (ppat_construct ~loc
           {
             txt = Lident (Utils.constructor_name_of_field ~field_name ~name);
             loc;
           }
           None)
      ~guard:None
      ~rhs:
        (pexp_construct ~loc (Utils.some_lident ~loc)
           (Some
              (pexp_record ~loc
                 [
                   ( { txt = Lident field_name; loc },
                     pexp_ident ~loc { txt = Lident "x"; loc } );
                 ]
                 (Some (pexp_ident ~loc { txt = Lident name; loc })))))

  let generate_setter_cases ~fields ~loc ~name =
    List.rev
      (case ~lhs:(ppat_any ~loc) ~guard:None
         ~rhs:(pexp_construct ~loc (Utils.none_lident ~loc) None)
      :: List.rev_map (fun field -> generate_setter_case ~field ~name) fields)

  let generate_setter ~fields ~name ~loc =
    value_binding ~loc
      ~pat:
        (ppat_constraint ~loc
           (ppat_var ~loc { txt = Utils.setter_name ~name; loc })
           (ptyp_constr ~loc (Utils.setter_lident ~loc) []))
      ~expr:
        (pexp_record ~loc
           [
             ( { txt = Lident "f"; loc },
               pexp_newtype ~loc { txt = "a"; loc }
                 (pexp_newtype ~loc { txt = "b"; loc }
                    (pexp_fun ~loc Nolabel None
                       (ppat_constraint ~loc
                          (ppat_var ~loc { txt = name; loc })
                          (ptyp_constr ~loc { txt = Lident "a"; loc } []))
                       (pexp_fun ~loc Nolabel None
                          (ppat_constraint ~loc
                             (ppat_var ~loc { txt = "field"; loc })
                             (ptyp_constr ~loc (Utils.field_lident ~loc)
                                [
                                  ptyp_constr ~loc { txt = Lident "a"; loc } [];
                                  ptyp_constr ~loc { txt = Lident "b"; loc } [];
                                ]))
                          (pexp_fun ~loc Nolabel None
                             (ppat_constraint ~loc
                                (ppat_var ~loc { txt = "x"; loc })
                                (ptyp_constr ~loc { txt = Lident "b"; loc } []))
                             (pexp_constraint ~loc
                                (pexp_match ~loc
                                   (pexp_ident ~loc
                                      { txt = Lident "field"; loc })
                                   (generate_setter_cases ~loc ~fields ~name))
                                (ptyp_constr ~loc (Utils.option_lident ~loc)
                                   [
                                     ptyp_constr ~loc { txt = Lident "a"; loc }
                                       [];
                                   ])))))) );
           ]
           None)

  let generate_add ~fields ~name ~loc =
    pstr_value ~loc Nonrecursive
      (List.map
         (fun { pld_name = { txt = field_name; _ }; pld_loc = loc; _ } ->
           value_binding ~loc
             ~pat:
               (ppat_constraint ~loc (ppat_any ~loc)
                  (ptyp_constr ~loc { txt = Lident "unit"; loc } []))
             ~expr:
               (pexp_apply ~loc
                  (pexp_ident ~loc (Utils.add_lident ~loc))
                  [
                    ( Nolabel,
                      pexp_construct ~loc
                        {
                          txt =
                            Lident
                              (Utils.constructor_name_of_field ~field_name ~name);
                          loc;
                        }
                        None );
                    ( Nolabel,
                      pexp_ident ~loc
                        { txt = Lident (Utils.getter_name ~name); loc } );
                    ( Nolabel,
                      pexp_ident ~loc
                        { txt = Lident (Utils.setter_name ~name); loc } );
                  ]))
         fields)

  let generate_getter_and_setter ~fields ~name ~loc =
    pstr_value ~loc Recursive
      [ generate_getter ~fields ~name ~loc; generate_setter ~fields ~name ~loc ]

  let generate ~ctxt:_ (_rec_flag, type_declarations) =
    List.fold_left
      (fun acc -> function
        | {
            ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
            ptype_loc = loc;
            _;
          } ->
            let ext =
              Location.error_extensionf ~loc
                "Cannot derive diffs for non-record types"
            in
            pstr_extension ~loc ext [] :: acc
        | {
            ptype_kind = Ptype_record fields;
            ptype_loc = loc;
            ptype_name = { txt = name; _ };
            _;
          } as td ->
            let ct = core_type_of_type_declaration td in
            generate_pre_attr ~loc
            :: generate_fields ~fields ~loc ~name ~ct
            :: generate_getter_and_setter ~fields ~name ~loc
            :: generate_add ~fields ~name ~loc
            :: generate_post_attr ~loc :: acc)
      [] type_declarations
end

let str_type_decl = Deriving.Generator.V2.make_noarg Impl.generate
let deriver = Deriving.add "diff" ~str_type_decl
