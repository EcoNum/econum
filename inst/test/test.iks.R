# Test IKS aquaqtar nouvelle generation
library(econum)
#ls /dev/cu*
options(iks.port = "/dev/cu.usbserial-FTB3O67T")
iks.open()
iks.getAll()
iks.getName()
iks.getData()
iks.getConfig()

options(debug.IKS = TRUE)
options(debug.IKS = FALSE)

iks.close()
