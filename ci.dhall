let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

let steps = haskellCi.defaultCabalSteps

in    haskellCi.generalCi
        ( steps
          with extraSteps.pre
               =
                steps.extraSteps.pre
              # [ haskellCi.BuildStep.Name
                    { name = "Native dependencies"
                    , run =
                        "sudo apt install libgmp-dev libgl1-mesa-dev libxcursor-dev libxi-dev libxinerama-dev libxrandr-dev libxxf86vm-dev"
                    }
                ]
        )
        (haskellCi.DhallMatrix::{=} with ghc = haskellCi.defaultGHC3)
    : haskellCi.CI.Type
