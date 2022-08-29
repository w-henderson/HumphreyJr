open Unix
open Request
open Response

type handler = string * (request -> response)

class app =
  object (self)
    val mutable handlers : handler list = []
    method add_route route handler = handlers <- (route, handler) :: handlers

    method request_handler in_channel out_channel =
      let request = parse_request in_channel in
      let handler =
        List.find_opt (fun (route, _) -> route = request.url) handlers
      in
      let response =
        match handler with
        | Some (_, handler) -> handler request
        | None -> response 404 "Not Found"
      in
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
        exit 0

    method start port =
      let sock = socket PF_INET SOCK_STREAM 0 in
      bind sock (ADDR_INET (inet_addr_any, port));
      listen sock 16;
      let rec loop () =
        let stream, _ = accept sock in
        match fork () with 0 -> self#stream_handler stream | _ -> loop ()
      in
      loop ();
      ()
  end
