
.onAttach <- function(libname, pkgname) {

vers <- utils::packageDescription("dbR6", fields = "Version")
textstart <- paste("
   _________________________________________
  |_________________________________________|
  |      _    _                             |
  |     | |  | |          _ ____    _ __    |
  |   __| |  | |__     _ _ /*_  \\ _  /__    |
  |  / _ *|  |* _ \\   _ _ /=/_/_/ _ /__/    |
  | | |_|||  |||_| | _ _ / /| |             |
  |  \\____|  |____/   _ /_/ |_|             |
  |                                         |
  -------------------------------------------
   ")


packageStartupMessage(textstart)
packageStartupMessage("   Version ", vers)

  where <- as.environment("package:dbR6")
  try(setOldClass(c("dbR6", "R6"), where = where), silent = TRUE)
}
