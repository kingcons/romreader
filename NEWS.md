## Changes for 0.5 (2013-xx-xx):

* INCOMPATIBLE CHANGE: Split the `:mapper` from rom-metadata into
  `:mapper-id` and `:mapper-name`.

## Changes for 0.4 (2013-02-19):

* Improved error reporting in load-rom.
* Added ROM-PRG and ROM-CHR methods to retrieve program and character/sprite data.
* Compute and store the size of PRG and CHR banks in ROM-METADATA.
* All rom instances are now a subclass of ROM.
* Added BSD license.

## Changes for 0.3 (2012-08-08):

* New ROM formats are defined via a new macro, DEFREADER.
* Exported ROM class and ROMREADER-ERROR condition.
* Fleshed out README.

## Changes for 0.2 (2012-07-06):

* Added formal API docs.

## Changes for 0.1 (2011-10-10):

* Initial release.
