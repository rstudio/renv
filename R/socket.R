
# avoid R CMD check errors with older R
if (getRversion() < "4.0") {
  utils::globalVariables(c("serverSocket", "socketAccept"))
}

renv_socket_server <- function(min = 49152, max = 65535) {

  # create the socket server
  port <- socket <- NULL
  for (i in 1:2000) catch({
    port <- sample(min:max, size = 1L)
    socket <- serverSocket(port)
    break
  })

  # if we still don't have a socket here, we failed
  if (is.null(socket))
    stop("error creating socket server: couldn't find open port")

  # return information about the server
  list(
    socket = socket,
    port = port,
    pid = Sys.getpid()
  )
}

renv_socket_connect <- function(port, open, timeout = getOption("timeout")) {
  socketConnection(
    host = "127.0.0.1",
    port = port,
    open = open,
    blocking = TRUE,
    encoding = "native.enc",
    timeout = timeout
  )
}

renv_socket_accept <- function(socket, open, timeout = getOption("timeout")) {
  socketAccept(
    socket = socket,
    open = open,
    blocking = TRUE,
    encoding = "native.enc",
    timeout = timeout
  )
}
