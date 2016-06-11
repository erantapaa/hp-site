{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Linux download section.

module Render.Linux where

import Control.Monad
import qualified Render.Base as RB
import Render.Base (DistroIcon(..), distro_svg)
import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

linux_distros :: [(String, String, DistroIcon)]
linux_distros =
    [ ("#linux-generic", "Generic", FontAwesome "fa fa-cogs")
    , ("#linux-ubuntu", "Ubuntu", Image "Ubuntu logo" (distro_svg "ubuntu"))
    , ("#linux-redhat", "RedHat", Image "Redhat logo" (distro_svg "redhat"))
    , ("#linux-debian", "Debian", Image "Debian logo" (distro_svg "debian"))
    , ("#linux-fedora", "Fedora", Image "Fedora logo" (distro_svg "fedora"))
    , ("#linux-mint",   "Mint",   Image "Linut Mint logo" (distro_svg "mint"))
    , ("#linux-gentoo", "Gentoo", Image "Gentoo Linux logo" (distro_svg "gentoo"))
    , ("#linux-source",  "From Source",  FontAwesome "fa fa-source")
    ]

linux_download :: [RB.FileInfo] -> [RB.FileInfo] -> Html
linux_download binFiles srcFiles = do

  H.section ! class_ "downloads-platform container preferred-platform visible" ! dataAttribute "os" "linux" ! A.id "linux" $ do
      H.div ! class_ "platform-name" $ do
          img ! RB.hl_src "platform/img/os-linux.svg" ! alt "Linux logo"
          h2 "Linux"

      RB.expander "#linux"

      H.div ! class_ "sidebar flavors" $ do
          strong "Choose your distribution"
          ul ! class_ "choose-distrbution" $ do

              RB.distro_button_list linux_distros

          p ! class_ "select-generic" $ do
              "If you can't find your distribution then select "
              strong "Generic"
              "."
      H.div ! class_ "content" $ do
          H.div ! A.id "linux-prompt" ! class_ "flavor active" $ do
              h3 "Select your distribution"
              p "Please select your Linux distribution in the list on the right. Note that distribution-packaged versions are typically behind the current platform release. If you prefer to use the latest version rather than the distribution-packaged version, then you may use the generic Linux installer."
              H.div ! class_ "point-to-flavors-list" $ i ! class_ "fa fa-arrow-right" $ mempty
          H.div ! A.id "linux-generic" ! class_ "flavor" $ do
              h3 "Generic Linux"
              p $ do
                  "The "
                  strong "generic"
                  " distribution of the Haskell Platform is designed to work with any modern Linux."
              p $ do
                  "The latest version of the Haskell Platform for Linux is "
                  strong "8.0.1"
                  "."
              p "To get started perform these steps,"
              ol ! class_ "install-steps" $ do
                  li $ do
                      H.div ! class_ "step-number" $ "1"
                      H.div ! class_ "step-body" $ do
                          p "Select a version:"

                          -- download a bin file
                          RB.downloadButtonsAndHashes binFiles

                  li $ do
                      H.div ! class_ "step-number" $ "2"
                      H.div ! class_ "step-body" $ do
                          "Complete installation by running:"
                          pre "$ tar xf ...downloaded archive...\n$ sudo ./install-haskell-platform.sh"
          --  #linux-generic 
          H.div ! A.id "linux-ubuntu" ! class_ "flavor" $ do
              h3 "Ubuntu"
              p $ do
                  "The Haskell Platform may be installed using "
                  code "apt-get"
                  " with the following command:"
              pre "$ sudo apt-get install haskell-platform haskell-platform-doc haskell-platform-prof"
              p $ do
                  "Please consult the "
                  a ! href "http://packages.ubuntu.com/search?keywords=haskell-platform" $ "Ubuntu repository"
                  " for the version of the Haskell Platform available for your system."
          --  #linux-ubuntu 
          H.div ! A.id "linux-debian" ! class_ "flavor" $ do
              h3 "Debian"
              p $ do
                  "The Haskell Platform may be installed using "
                  code "apt-get"
                  " with the following command:"
              pre "$ sudo apt-get install haskell-platform haskell-platform-doc haskell-platform-prof"
              p $ do
                  "Please consult the "
                  a ! href "https://packages.debian.org/search?keywords=haskell-platform" $ "Debian repository"
                  " for the version of the Haskell Platform available for your system."
          --  #linux-debian 
          H.div ! A.id "linux-mint" ! class_ "flavor" $ do
              h3 "Linux Mint"
              p $ do
                  "The Haskell Platform may be installed using "
                  code "apt-get"
                  " with the following command:"
              pre "$ sudo apt-get install haskell-platform"
              p $ do
                  "Please consult the "
                  a ! href "http://community.linuxmint.com/software/view/haskell-platform" $ "Mint repository"
                  " for the version of the Haskell Platform available for your system."
          --  #linux-mint 
          H.div ! A.id "linux-redhat" ! class_ "flavor" $ do
              h3 "Redhat"
              p $ do
                  "The Haskell Platform may be installed using"
                  " " >> code "yum" >> " "
                  "with the following command:"
              pre "$ sudo yum install haskell-platform"
              p $ do
                  "Please consult the Red Hat package repository for the version of the Haskell Platform available for your system."
          --  #linux-redhat 
          H.div ! A.id "linux-fedora" ! class_ "flavor" $ do
              h3 "Fedora"
              p $ do
                  "The Haskell Platform may be installed using the " >> code "dnf" >> " "
                  "command with:"
              pre "$ sudo dnf install haskell-platform"
              p $ do
                  "Please consult the "
                  a ! href "https://admin.fedoraproject.org/pkgdb/package/haskell-platform/" $ "Fedora repository"
                  " for the version of the Haskell Platform available for your system."
          --  #linux-fedora 
          H.div ! A.id "linux-gentoo" ! class_ "flavor" $ do
              h3 "Gentoo"
              p $ do
                  "While there is a "
                  code "haskell-platform"
                  " ebuild included in the main Portage tree, it is recommended that one uses the more up-to-date"
                  a ! href "https://github.com/gentoo-haskell/gentoo-haskell/tree/master/dev-haskell/haskell-platform" $ " " >> code "gentoo-haskell" >> " "
                  "overlay. This can be done using "
                  code "layman"
                  ":"
              pre "$ sudo layman -a haskell\n$ sudo emerge haskell-platform"
              p $ do
                  "More details can be found in the "
                  a ! href "https://wiki.haskell.org/Gentoo/HaskellPlatform" $ "Wiki"
                  "."
          --  #linux-gentoo 
          H.div ! A.id "linux-source" ! class_ "flavor" $ do
              h3 "Build from source"
              p "To build the Haskell Platform from source:"
              ol ! class_ "install-steps" $ do
                  li $ do
                      H.div ! class_ "step-number" $ "1"
                      H.div ! class_ "step-body" $ do
                          p "Download the source archive:"

                          -- Download a source file
                          RB.downloadButtonsAndHashes srcFiles

                  li $ do
                      H.div ! class_ "step-number" $ "2"
                      H.div ! class_ "step-body" $ p $ do
                          "Unpack the archive and consult the "
                          code "README"
                          " file for build instructions."
          --  #linux-source 
      --  linux .content 
      H.div ! class_ "bottom-rule" $ mempty

