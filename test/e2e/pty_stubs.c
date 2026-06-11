#include <string.h>
#include <pty.h>
#include <utmp.h>
#include <sys/ioctl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

CAMLprim value ine_openpty(value vw, value vh)
{
  CAMLparam2(vw, vh);
  CAMLlocal1(res);
  int master, slave;
  struct winsize ws;
  memset(&ws, 0, sizeof ws);
  ws.ws_col = Int_val(vw);
  ws.ws_row = Int_val(vh);
  if (openpty(&master, &slave, NULL, NULL, &ws) == -1)
    uerror("openpty", Nothing);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_int(master));
  Store_field(res, 1, Val_int(slave));
  CAMLreturn(res);
}

CAMLprim value ine_login_tty(value vfd)
{
  if (login_tty(Int_val(vfd)) == -1)
    uerror("login_tty", Nothing);
  return Val_unit;
}

/* Applying TIOCSWINSZ to the pty also delivers SIGWINCH to the foreground
   process group, like a real terminal resize. */
CAMLprim value ine_set_winsize(value vfd, value vw, value vh)
{
  struct winsize ws;
  memset(&ws, 0, sizeof ws);
  ws.ws_col = Int_val(vw);
  ws.ws_row = Int_val(vh);
  if (ioctl(Int_val(vfd), TIOCSWINSZ, &ws) == -1)
    uerror("ioctl(TIOCSWINSZ)", Nothing);
  return Val_unit;
}
