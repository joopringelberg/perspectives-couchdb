
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220404/packages.dhall
        sha256:75d0f0719f32456e6bdc3efd41cfc64785655d2b751e3d080bd849033ed053f2

in  upstream
  with avar-monadask =
    { dependencies =
      [ "prelude"
      , "avar"
      , "aff"
      , "transformers"
      ]
    , repo =
       "https://github.com/joopringelberg/purescript-avar-monadask.git"
    , version =
        "v2.2.0"
    }
  with affjax.repo = "https://github.com/joopringelberg/purescript-affjax.git"
  with affjax.version = "v12.0.0-with-xhr-cookies"
