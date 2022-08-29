open HumphreyJr

let index_handler (_, state) =
  let response =
    Response.response 200
      ("This is the home page! It has been visited " ^ string_of_int state
     ^ " times")
  in
  let new_state = state + 1 in
  (response, new_state)

let contact_handler _ = Response.response 200 "This is the contact page!"

let () =
  let app = new App.app 0 in
  app#add_route "/" index_handler;
  app#add_stateless_route "/contact" contact_handler;
  app#start 8000
