# Send plots drawn during the tests to a null device, so that no Rplots.pdf
# is left in the tests folder
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), teardown_env())
