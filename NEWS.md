# econum News

## Changes in econum 1.2.0

* Bug corrected: missmatch between localRepos (old name) and local_repos
  (new name) for the corresponding otion. Idem for remoteRepos/remote_repos.

* new_econum_data() now works with missing metadata().

* repos_load() now passes ... to validate().


## Changes in econum 1.1-7

* Rework of code.


## Changes in econum 1.1-6

* `iks.process()` now also accepts lowercase names for probes (as of more recent
  IKS aquastar devices provide them).


## Changes in econum 1.1-5

* In `iks.open()`, the Tcl proc installed to handle serial entries can lead to
  an error in Tcl while unsetting the corresponding variable in a very rare and
  weird case where the serial port is connected through a serial-to-usb adapter
  and the USB port deconnects just in the middle of a communication. The unset
  command is now embedded into a catch { unset ....} construct to by-pass in
  this rare situation.


## Changes in econum 1.1-4

* Code for `gce8Cmd()` is adapted to the new series of cards that implement the
  memory feature (M0/M1 instructions). If `relay = NULL` in `gce8Cmd()`, then,
  memory state is changed.


## Changes in econum 1.1-3

* IKS routines tested and adapted to Linux Ubuntu 11.10

* `timeFingerprint()` renamed `timeToFingerprint()` and the opposite function
  `fingerprintToTime()` added.

* Addition of `timeFingerprint()`, `optionsEcoNum()`, `getOptEcoNum` and
  `setOptEcoNum()` in utilities.R.

* Addition of `EcoNumData` object and related methods `print()`, `summary()`,
  `validate()`, `reposSave()`. Also addition of `reposLoad()` to reload some
  `EcoNumData` from the local or remote repository.


## Changes in econum 1.1-2

* `.iks.port()` is modified to allow com ports higher than 9 on Windows.


## Changes in econum 1.1-1

* `gce8Ready()` always took the default port or name, corrected.


## Changes in econum 1.1-0

* Instructions are added to control a GCE USB 8 relays card through a virtual
  comm port (functions gce8*).


## econum 1.0-0

First version of the package.
