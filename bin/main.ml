open HumphreyJr

let ws_handler ws =
  Websocket.send ws "hello world";
  let reply = Websocket.recv ws |> Bytes.to_string in
  print_endline reply

let () =
  let port =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 8000
  in
  let app = new App.app () in
  app#add_static_route "/*" ".";
  app#add_websocket_route "/*" ws_handler;
  app#start port
