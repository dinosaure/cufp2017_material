open Tyxml.Html
open Cmdliner

let d3js = "https://d3js.org/d3.v4.min.js"
let graphjs = "graph.js"

let script = function
  | None -> html (head (title (pcdata "Index")) []) (body [])
  | Some s ->
    html
      (head
       (title (pcdata "Index"))
       [ meta ~a:[ a_http_equiv "content-type"
                 ; a_content "text/html; charset=utf-8"] () ])
      (body ~a:[ a_id "main" ]
         [ svg ~a:[ Tyxml.Svg.a_width (1024., None) 
                  ; Tyxml.Svg.a_height (768., None)
                  ; Tyxml.Svg.a_style "position: fixed" ] []
         ; script ~a:[ a_src @@ Xml.uri_of_string d3js ] @@ pcdata ""
         ; script ~a:[ a_src @@ Xml.uri_of_string graphjs ] @@ pcdata ""
         ; script ~a:[ a_src @@ Xml.uri_of_string s ] @@ pcdata "" ])

let main s =
  let c = script s in
  Format.printf "%a%!" (pp ()) c;
  `Ok ()

let script =
  let doc = "Script JS" in
  Arg.(value & opt (some string) None & info ["s"; "script"] ~docv:"script" ~doc)

let cmd =
  let doc = "Rendering INDEX page" in
  let man =
  [ `P "BUGS"
  ; `S "Email them to <romain.calascibetta@gmail.com>" ]
  in Term.(ret (pure main $ script)), Term.info "to_html" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _        -> exit 0
