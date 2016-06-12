{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Windows download section

module Render.Windows where

import Control.Monad
import qualified Render.Base as RB
import Render.Base (DistroIcon(..), distro_svg)
import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

windows_download binFiles = do
    section ! A.id "windows-section" $ do
        H.div ! class_ "platform-name" $ do
            img ! RB.hl_src "platform/img/os-windows.svg" ! alt "Windows logo"
            h2 "Windows"

        RB.expander "#windows-section" "windows-expander"

        H.div ! A.id "windows-content" ! class_ "content" $ do
            p $ do
                "The latest version of the Haskell Platform for Windows is "
                strong "8.0.1"
                "."
            p "To get started perform these steps,"
            ol ! class_ "install-steps" $ do
                li $ do
                    H.div ! class_ "step-number" $ "1"
                    H.div ! class_ "step-body" $ do
                        p "Select an installer:"

                        -- download a bin file
                        RB.downloadButtonsAndHashes binFiles

                li $ do
                    H.div ! class_ "step-number" $ "2"
                    H.div ! class_ "step-body" $ "Run the installer."
                li $ do
                    H.div ! class_ "step-number" $ "3"
                    H.div ! class_ "step-body" $ do
                        "After the installer finishes, run "
                        code $ "cabal user-config init"
                        " to locate your cabal config file. "
                        p $ "Modify the config file to contain the following lines:"
                        pre "extra-prog-path: C:\\Program Files\\Haskell Platform\\8.0.1\\msys\\usr\\bin\nextra-lib-dirs: C:\\Program Files\\Haskell Platform\\8.0.1\\mingw\\lib\nextra-include-dirs: C:\\Program Files\\Haskell Platform\\8.0.1\\mingw\\include"
                li $ do
                    H.div ! class_ "step-number" $ "4"
                    H.div ! class_ "step-body" $ "Start WinGHCi from the Start menu and have fun!"

