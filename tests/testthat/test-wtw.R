context("WTW instruments control")

test_that("Port is correct", {
  oport <- options(wtw_port = "com0") # Change this to connect to a real instrument

  expect_true(wtw_open())

  # TODO: test wtw_open("com500") returns an error
  # TODO: test these
  #wtw_echo()	# Echo data send by run/enter from the instrument
  #wtw_read()  # Read data from the instrument
  #wtw_ready() # Check that instrument is ready
  #wtw_close()
  #wtw_ready()	# Not ready any more, because we closed the port

  ## Record 10 data with an interval of 1 sec and make a graph...
  ## but speed up things by ten times if we use a fake instrument
  #if (getOption("wtw_port") == "com0") options(econum_speed = 10)
  ## Note that you don't have to open/close the port with this function!
  #wtw_record(interval = 1, Nmax = 10)
  ## Restore default speed
  #options(econum_speed = NULL)

  # Restore port
  options(oport); rm(oport)
})
