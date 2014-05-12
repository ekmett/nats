0.2
---
* Added `Hashable` and `Data` support.
* Will build as full-fledged `Safe` Haskell if you configure with -f-hashable, merely `Trustworthy` otherwise.
* Allow for manual removal of the `hashable` dependency to support advanced sandbox users who explicitly want to avoid compiling certain dependencies
  they know they aren't using.

  We will fix bugs caused by any combination of these package flags, but the API of the package should be considered the default build
  configuration with all of the package dependency flags enabled.

0.1.3
-----
* Added support for GHC 7.8's `bitSizeMaybe`

0.1
---
* Repository Initialized moving `Numeric.Natural` from `semigroups` 0.8.6
