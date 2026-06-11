external openpty : w:int -> h:int -> Unix.file_descr * Unix.file_descr
  = "ine_openpty"

external login_tty : Unix.file_descr -> unit = "ine_login_tty"

external set_winsize : Unix.file_descr -> w:int -> h:int -> unit
  = "ine_set_winsize"
