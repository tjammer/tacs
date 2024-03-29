module C = Configurator.V1

let () =
  C.main ~name:"raylib" (fun c ->
      let link_flags =
        match C.ocaml_config_var c "system" with
        | Some "mingw64" ->
            [
              "-cclib";
              "-Wl,-static";
              "-cclib";
              "-subsystem";
              "-cclib";
              "windows";
              "-cclib";
              "-Wl,-s";
            ]
        | Some _ | None -> [ "-cclib"; "-s" ]
      in

      C.Flags.write_sexp "link_flags.sexp" link_flags)
