# Dhall configuration files

NOTE: there seems to be a bug with the dhall linter, causing it to delete
comments. So I've started keeping notes in README files in all the various
directories inside the dhall configuration tree.


CoreName must be a valid dhall label, Name only has to be valid in KMonad

Name:
- starts with lowercase letter or underscore
- after that, anything goes except ;, which *must* be escaped

CoreName:
- much more restricted subset of valid characters


