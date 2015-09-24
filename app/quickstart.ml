let resources =
  let open LTerm_resources in
  empty
  |> add "label.foreground" "blue"

let ui term =
  let open LTerm_widget in
  let waiter, wakener = Lwt.wait () in

  let vbox = new vbox in
  let label = new label "biquette" in
  label#set_resources resources ;
  vbox#add label ;
  vbox#on_event (function
      | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape } -> Lwt.wakeup wakener (); true
      | _ -> false
    ) ;
  vbox#set_resources resources ;
  run term vbox waiter

let main_thread () =
  lwt term = Lazy.force LTerm.stdout in
  ui term

let main copts output =
  Lwt_unix.run (main_thread ())
