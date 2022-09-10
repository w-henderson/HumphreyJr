open HumphreyJr

let () =
  let port =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 8000
  in
  let app = new App.app () in
  app#add_static_route "/*" ".";
  app#start port
