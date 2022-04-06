{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "perspectives-couchdb"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "avar-monadask"
  , "affjax"
  , "argonaut"
  , "foreign-generic"
  , "simple-json"
  , "b64"
  , "test-unit"
  , "aff-promise"
  , "aff"
  , "aff-coroutines"
  , "arrays"
  , "avar"
  , "coroutines"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "free"
  , "http-methods"
  , "maybe"
  , "media-types"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
