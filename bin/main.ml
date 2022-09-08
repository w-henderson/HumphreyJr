open HumphreyJr
open HumphreyJr.Request

let index_handler (_, state) =
  let response =
    Response.response 200
      ("This is the home page! It has been visited " ^ string_of_int state
     ^ " times")
  in
  let new_state = state + 1 in
  (response, new_state)

let wild_handler request = Response.response 200 ("URL: " ^ request.url)

let () =
  let app = new App.app 0 in
  app#add_route "/" index_handler;
  app#add_stateless_route "/wild*" wild_handler;
  app#add_static_route "/static/*" "/mnt/c/var/www";
  app#start 8000
