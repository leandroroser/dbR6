
.onAttach <- function(libname, pkgname) {

vers <- utils::packageDescription("dbR6", fields = "Version")
textstart <- paste("
          _    _
         | |  | |        _____      ___
       __| |  | |__     |  _  \\    / _ \\
      / _  |  |  _ \\    |_|_|_|   / |_
     | |_|||  |||_| |   | |\\ \\    | |_| \\
      \\____|  |____/    |_| \\_\\   \\_____/
   ")


packageStartupMessage(textstart)
packageStartupMessage("     Version ", vers)

  where <- as.environment("package:dbR6")
  try(setOldClass(c("dbR6", "R6"), where = where), silent = TRUE)
}
