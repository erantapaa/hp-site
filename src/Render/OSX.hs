{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the OSX download section

module Render.OSX where

import Control.Monad
import qualified Render.Base as RB
import Render.Base (distro_png, DistroIcon(..))
import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

osx_distro_buttons =
   [ ("#osx-none",         "None",     FontAwesome "fa fa-cogs")
   , ("#osx-macports",     "MacPorts", Image "MacPorts logo" (distro_png "macports"))
   , ("#osx-homebrewcask", "Homebrew", Image "MacPorts logo" (distro_png "homebrew"))
   ]

osx_download binFiles = do
    section ! class_ "downloads-platform container" ! dataAttribute "os" "osx" ! A.id "osx" $ do
        H.div ! class_ "platform-name" $ do
            img ! RB.hl_src "platform/img/os-osx.svg" ! alt "Mac OS X logo"
            h2 "Mac OS X"

        RB.expander "#osx"

        H.div ! class_ "sidebar flavors" $ do
            strong "Choose your package manager"

            RB.distro_button_list osx_distro_buttons

        H.div ! class_ "content" $ do
            H.div ! A.id "osx-none" ! class_ "flavor active" $ do
                p $ do
                    "The latest version of the Haskell Platform for Mac OS X is "
                    strong "8.0.1 and requires " >> strong "OS X 10.6 or later" >> "."
                p "These packages are for Mac OS X systems not using a package manager. If you would rather install with MacPorts or Homebrew then select the appropriate option to the right. (Note that those distributions may lag behind official platform installers)."
                p "To get started perform these steps,"
                ol ! class_ "install-steps" $ do
                    li $ do
                        H.div ! class_ "step-number" $ "1"
                        H.div ! class_ "step-body" $ do
                            "Select a PKG installer:"
                            p mempty

                            -- download a bin file
                            RB.downloadButtonsAndHashes binFiles

                    li $ do
                        H.div ! class_ "step-number" $ "2"
                        H.div ! class_ "step-body" $ "Run the installer."
                    li $ do
                        H.div ! class_ "step-number" $ "3"
                        H.div ! class_ "step-body" $ "Follow the instructions."
            --  #osx-none 
            H.div ! A.id "osx-macports" ! class_ "flavor" $ do
                h3 "MacPorts"
                p $ do
                    "To install Haskell Platform with "
                    a ! href "https://trac.macports.org/browser/trunk/dports/devel/haskell-platform/Portfile" $ "MacPorts"
                    " run the command:"
                pre "$ sudo port install haskell-platform"
            --  #osx-macports 
            H.div ! A.id "osx-homebrewcask" ! class_ "flavor" $ do
                h3 "Homebrew Cask"
                p $ do
                    "To install Haskell Platform with "
                    a ! href "http://caskroom.io" $ "Homebrew Cask"
                    " run the command:"
                pre "$ brew cask install haskell-platform"
            --  #osx-homebrewcask 
        H.div ! class_ "bottom-rule" $ mempty

