external openpty : w:int -> h:int -> Unix.file_descr * Unix.file_descr
  = "cud_openpty"

external login_tty : Unix.file_descr -> unit = "cud_login_tty"

external set_winsize : Unix.file_descr -> w:int -> h:int -> unit
  = "cud_set_winsize"
