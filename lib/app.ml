open Unix
open Request
open Response
open Wildcard
open Websocket
open Handshake

type 'state handler = string * (request * 'state -> response * 'state)
type websocket_handler = string * (websocket -> unit)

class ['state] app state =
  object (self)
    val mutable handlers : 'state handler list = []
    val mutable websocket_handlers : websocket_handler list = []
    val mutable state : 'state = state
    val state_lock = Mutex.create ()
    method add_route route handler = handlers <- (route, handler) :: handlers

    method add_websocket_route route handler =
      websocket_handlers <- (route, handler) :: websocket_handlers

    method add_stateless_route route handler =
      handlers <- (route, fun (req, state) -> (handler req, state)) :: handlers

    method add_static_route route directory =
      self#add_stateless_route route (Static.handle route directory)

    method websocket_handler request in_channel out_channel =
      handshake request out_channel;
      let handler =
        List.find_opt
          (fun (route, _) -> wildcard_match route request.url)
          websocket_handlers
      in
      match handler with
      | Some (_, handler) -> handler { in_channel; out_channel }
      | None -> ()

    method request_handler in_channel out_channel =
      let request = parse_request in_channel in
      if
        List.exists
          (fun (k, v) -> k = "Upgrade" && v = "websocket")
          request.headers
      then self#websocket_handler request in_channel out_channel
      else
        let handler =
          List.find_opt
            (fun (route, _) -> wildcard_match route request.url)
            handlers
        in
        Mutex.lock state_lock;
        let response, new_state =
          match handler with
          | Some (_, handler) -> handler (request, state)
          | None -> (response 404 "Not Found", state)
        in
        state <- new_state;
        Mutex.unlock state_lock;
        let is_keep_alive = is_keep_alive request in
        let response = set_keep_alive response is_keep_alive in

        let bytes = bytes_of_response response in
        output_bytes out_channel bytes;
        flush out_channel;

        match is_keep_alive with
        | true -> (
            try self#request_handler in_channel out_channel
            with End_of_file -> ())
        | false -> ()

    method stream_handler stream =
      let in_channel = in_channel_of_descr stream in
      let out_channel = out_channel_of_descr stream in
      self#request_handler in_channel out_channel;

      shutdown stream SHUTDOWN_ALL;
      close_out out_channel;
      try
        close_in in_channel;
        close stream
      with _ ->
        ();
        Thread.exit ()

    method start port =
      let sock = socket PF_INET SOCK_STREAM 0 in
      bind sock (ADDR_INET (inet_addr_any, port));
      listen sock 16;
      let rec loop () =
        let stream, _ = accept sock in
        let _ = Thread.create self#stream_handler stream in
        loop ();
        ()
      in
      loop ()
  end
