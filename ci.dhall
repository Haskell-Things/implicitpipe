let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        (   [ haskellCi.BuildStep.Name
                { name = "Native dependencies"
                , run =
                    "sudo apt install libgmp-dev libgl1-mesa-dev libxcursor-dev libxi-dev libxinerama-dev libxrandr-dev libxxf86vm-dev"
                }
            ]
          # haskellCi.matrixSteps
        )
        ( Some
            { ghc = [ haskellCi.GHC.GHC902, haskellCi.GHC.GHC8107 ]
            , cabal = [ haskellCi.Cabal.Cabal34 ]
            }
        )
    : haskellCi.CI.Type
