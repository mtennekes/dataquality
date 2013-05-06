require(devtools)

## Create palletes, vignette and documentation...
setwd("../build")
source("load_samplon_data.R")
source("roxygen.R")
setwd("../pkg")


++++++## check
if (check()) {
	unlink( '../output', TRUE)
	dir.create("../output", showWarnings=FALSE)
	build(path= "../output")
}
