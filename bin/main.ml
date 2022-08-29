open HumphreyJr

let index_handler _ = Response.response 200 "This is the home page!"
let contact_handler _ = Response.response 200 "This is the contact page!"

let () =
  let app = new App.app in
  app#add_route "/" index_handler;
  app#add_route "/contact" contact_handler;
  app#start 8000
