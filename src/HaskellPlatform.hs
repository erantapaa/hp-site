{-# LANGUAGE OverloadedStrings #-}

module HaskellPlatform
where

import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A
import HaskellOrg (hl_src, hl_href)

hp_head = do
    link ! hl_href  "/platform/stylesheets/download.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css" ! rel "stylesheet" ! type_ "text/css"
    H.style $ do
      ".hp-branding { font-family: sans-serif; line-height: 50px; font-weight: bold; font-size: 50px; background-repeat: no-repeat; background-size: 70px; display: block; padding-left: 80px; background-position: left; } "
      ".hp-summary { margin-top: 20px; display: block; font-size: 20px; }"
    script ! hl_src "/platform/js/jquery-1.11.1.min.js" $ mempty
    script ! src "js/download.js" $ mempty

banner_left = do
    H.div $ do
        H.span ! A.style "background-image: url(img/logo.png)" ! class_ "hp-branding" $ "Haskell Platform"
        H.span ! class_ "hp-summary" $ "Haskell with batteries included"

banner_right = do
    h3 "A multi-OS distribution"
    p "designed to get you up and running quickly, making it easy to focus on using Haskell. You get:"
    ul $ do
        li $ do
            "the "
            a ! href "http://www.haskell.org/ghc/" $ "Glasgow Haskell Compiler"
        li $ do
            "the "
            a ! href "http://www.haskell.org/cabal/" $ "Cabal build system"
        li $ do
            "the "
            a ! href "http://docs.haskellstack.org" $ "Stack tool"
            " for developing projects"
        li "support for profiling and code coverage analysis"
        li $ do
            "35 core & widely-used "
            a ! href "contents.html" $ "packages"
    a ! href "prior.html" $ "Prior releases"
    " of the Platform are also available."

getting_started = do
    h2 ! A.id "get-started" $ "Let's get started"
    p $ do
        b "Note:"
        "as of 8.0.1 there are two download options available — minimal and full. The minimal option is currently the generally recommended one. It does not include any additional global libraries beyond those packaged with ghc, though it does include all tools. This ensures maximal compatibility with a variety of library sets. The full option is useful for those who prefer the \"classic\" platform behavior with a broader set of preinstalled libraries, and especially serves those well who want full-featured installers in situations where network connectivity should not be taken for granted."
    p $ do
        "Note also: the stack tool has been evolving relatively rapidly. Users who wish to ensure they are running the latest version may want to consider running \"stack update\" and ensuring the proper"
        a ! href "http://docs.haskellstack.org/en/stable/install_and_upgrade/#path" $ "path"
        "for stack-installed binaries is in their environment."

found_user_platform = do
    H.div ! class_ "container found-user-platform" $ do
        "You appear to be using "
        strong "Linux"
        ". See "
        a ! href "#other-platforms" $ "below"
        " for other operating systems."

unknown_user_platform = do
    H.div ! class_ "container unknown-user-platform" $ "Find your operating system of choice below and follow the instructions to install the Haskell Platform on your system."

platform_toc = do
    H.div ! class_ "container platform-toc" $ ul $ do
        li $ a ! href "#osx" $ do
            img ! hl_src "platform/img/os-osx.svg" ! alt "Mac OS X logo"
            "Mac OS X"
        li $ a ! href "#windows" $ do
            img ! hl_src "platform/img/os-windows.svg" ! alt "Windows logo"
            "Windows"
        li $ a ! href "#linux" $ do
            img ! hl_src "platform/img/os-linux.svg" ! alt "Linux logo"
            "Linux"


linux_download = do
  H.section ! class_ "downloads-platform container preferred-platform visible" ! dataAttribute "os" "linux" ! A.id "linux" $ do
      H.div ! class_ "platform-name" $ do
          img ! hl_src "platform/img/os-linux.svg" ! alt "Linux logo"
          h2 "Linux"
      a ! class_ "expander" ! href "#linux" $ H.div $ do
          img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-1"
          img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-2"
          img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-3"
      H.div ! class_ "sidebar flavors" $ do
          strong "Choose your distribution"
          ul $ do
              li $ a ! href "#linux-generic" $ do
                  H.span ! class_ "logo" $ i ! class_ "fa fa-cogs" $ mempty
                  "Generic"
              li $ a ! href "#linux-ubuntu" $ do
                  img ! alt "Ubuntu logo" ! class_ "logo" ! hl_src "platform/img/distro-ubuntu.svg"
                  "Ubuntu"
              li $ a ! href "#linux-debian" $ do
                  img ! alt "Debian logo" ! class_ "logo" ! hl_src "platform/img/distro-debian.svg"
                  "Debian"
              li $ a ! href "#linux-redhat" $ do
                  img ! alt "Redhat logo" ! class_ "logo" ! hl_src "platform/img/distro-redhat.svg"
                  "Redhat"
              li $ a ! href "#linux-fedora" $ do
                  img ! alt "Fedora logo" ! class_ "logo" ! hl_src "platform/img/distro-fedora.svg"
                  "Fedora"
              li $ a ! href "#linux-mint" $ do
                  img ! alt "Linux Mint logo" ! class_ "logo" ! hl_src "platform/img/distro-mint.svg"
                  "Mint"
              li $ a ! href "#linux-gentoo" $ do
                  img ! alt "Gentoo Linux logo" ! class_ "logo" ! hl_src "platform/img/distro-gentoo.svg"
                  "Gentoo"
              li $ a ! href "#linux-source" $ do
                  H.span ! class_ "logo" $ i ! class_ "fa fa-code" $ mempty
                  "From Source"
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
                  "This is a "
                  strong "generic"
                  " distribution of the Haskell Platform. While it should work on most modern Linux distributions, you may want to investigate use one of the distribution-specific options listed on the right."
              p $ do
                  "The latest version of the Haskell Platform for Linux is "
                  strong "8.0.1"
                  "."
              p "To get started perform these steps,"
              ol ! class_ "install-steps" $ do
                  li $ do
                      H.div ! class_ "step-number" $ "1"
                      H.div ! class_ "step-body" $ do
                          p "Download the installation tarball,"
                          H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/haskell-platform-8.0.1-unknown-posix--minimal-x86_64.tar.gz" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                              i ! class_ "fa fa-download" $ mempty
                              "Download Minimal (64 bit)"
                          H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/haskell-platform-8.0.1-unknown-posix--full-x86_64.tar.gz" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                              i ! class_ "fa fa-download" $ mempty
                              "Download Full (64 bit)"
                          H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/haskell-platform-8.0.1-unknown-posix--minimal-i386.tar.gz" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                              i ! class_ "fa fa-download" $ mempty
                              "Download Minimal (32 bit)"
                          H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/haskell-platform-8.0.1-unknown-posix--full-i386.tar.gz" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                              i ! class_ "fa fa-download" $ mempty
                              "Download Full (32 bit)"
                          H.div $ do
                              "You can verify the authenticity of this file by checking its "
                              strong "SHA-256"
                              "hash,"
                              ul ! class_ "hashes" $ do
                                  li $ do
                                      "64 bit Minimal:"
                                      input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "adec8e8f2e2440d7f506f1cb9aaf20496cd443660e55c0d588f28a0119171f8a"
                                  li $ do
                                      "64 bit Full:"
                                      input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "d747aaa51eb20a7c8b4de93fa2a0d07c3b54fc5f36bf50fcede1a332812656f7"
                                  li $ do
                                      "32 bit Minimal:"
                                      input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "1476ec7fda53654fe97118ded44333b091160fc5f4588c2ad7a0f8145c254d14"
                                  li $ do
                                      "32 bit Full:"
                                      input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "4643123f51401489d99302c150dc763f1d92614c428b921257b375f3895f7a79"
                  li $ do
                      H.div ! class_ "step-number" $ "2"
                      H.div ! class_ "step-body" $ do
                          "Install by running:"
                          pre "$ tar xf ...downloaded archive...\n$ sudo ./install-haskell-platform.sh"
          --  #linux-generic 
          H.div ! A.id "linux-ubuntu" ! class_ "flavor" $ do
              h3 "Ubuntu"
              p $ do
                  "Good news! Haskell Platform is already available in your distribution's package"
                  a ! href "http://packages.ubuntu.com/search?keywords=haskell-platform" $ "repository"
                  "."
              p "Simply run,"
              pre "$ sudo apt-get install haskell-platform"
          --  #linux-ubuntu 
          H.div ! A.id "linux-debian" ! class_ "flavor" $ do
              h3 "Debian"
              p $ do
                  "Good news! Haskell Platform is already available in your distribution's package"
                  a ! href "https://packages.debian.org/search?keywords=haskell-platform" $ "repository"
                  "."
              p "Simply run,"
              pre "$ sudo apt-get install haskell-platform"
          --  #linux-debian 
          H.div ! A.id "linux-mint" ! class_ "flavor" $ do
              h3 "Linux Mint"
              p $ do
                  "Good news! Haskell Platform is already available in your distribution's package"
                  a ! href "http://community.linuxmint.com/software/view/haskell-platform" $ "repository"
                  "."
              p "Simply run,"
              pre "$ sudo apt-get install haskell-platform"
          --  #linux-mint 
          H.div ! A.id "linux-redhat" ! class_ "flavor" $ do
              h3 "Redhat"
              p "Good news! Haskell Platform is already available in your distribution's package repository."
              p "Simply run,"
              pre "$ sudo yum install haskell-platform"
          --  #linux-redhat 
          H.div ! A.id "linux-fedora" ! class_ "flavor" $ do
              h3 "Fedora"
              p $ do
                  "Good news! Haskell Platform is already available in your distribution's package"
                  a ! href "https://admin.fedoraproject.org/pkgdb/package/haskell-platform/" $ "repository"
                  "."
              p "Simply run,"
              pre "$ sudo dnf install haskell-platform"
          --  #linux-fedora 
          H.div ! A.id "linux-gentoo" ! class_ "flavor" $ do
              h3 "Gentoo"
              p "Good news! Haskell Platform is already available in your distribution's package repository."
              p $ do
                  "While there is a"
                  code "haskell-platform"
                  "ebuild included in the main Portage tree, it is recommended that one uses the more up-to-date"
                  a ! href "https://github.com/gentoo-haskell/gentoo-haskell/tree/master/dev-haskell/haskell-platform" $ code "gentoo-haskell"
                  "overlay. This can be done using"
                  code "layman"
                  ","
              pre "$ sudo layman -a haskell\n$ sudo emerge haskell-platform"
              p $ do
                  "More details can be found in the"
                  a ! href "https://wiki.haskell.org/Gentoo/HaskellPlatform" $ "Wiki"
                  "."
          --  #linux-gentoo 
          H.div ! A.id "linux-source" ! class_ "flavor" $ do
              h3 "Build from source"
              p "If we don't have a binary package suitable for your distribution you can build the Haskell Platform from source."
              ol ! class_ "install-steps" $ do
                  li $ do
                      H.div ! class_ "step-number" $ "1"
                      H.div ! class_ "step-body" $ do
                          p "Download and extract the source tarball,"
                          H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/haskell-platform-8.0.1.tar.gz" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                              i ! class_ "fa fa-download" $ mempty
                              "Download"
                          H.div $ do
                              "You can verify the authenticity of this file by checking its "
                              strong "SHA-256"
                              "hash,"
                              ul ! class_ "hashes" $ li $ input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "38af99a9ae4afce56df75a753a19e7a4986bfbc8ce22f93b8308b5ab9e5a19c6"
                  li $ do
                      H.div ! class_ "step-number" $ "2"
                      H.div ! class_ "step-body" $ p $ do
                          "See the"
                          code "README"
                          "file for build instructions."
          --  #linux-source 
      --  linux .content 
      H.div ! class_ "bottom-rule" $ mempty

osx_download = do
    section ! class_ "downloads-platform container" ! dataAttribute "os" "osx" ! A.id "osx" $ do
        H.div ! class_ "platform-name" $ do
            img ! hl_src "platform/img/os-osx.svg" ! alt "Mac OS X logo"
            h2 "Mac OS X"
        a ! class_ "expander" ! href "#osx" $ H.div $ do
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-1"
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-2"
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-3"
        H.div ! class_ "sidebar flavors" $ do
            strong "Choose your package manager"
            ul $ do
                li ! class_ "active" $ a ! href "#osx-none" $ do
                    H.span ! class_ "logo" $ i ! class_ "fa fa-cogs" $ mempty
                    "None"
                li $ a ! href "#osx-macports" $ do
                    img ! alt "MacPorts logo" ! class_ "logo" ! hl_src "platform/img/distro-macports.png"
                    "MacPorts"
                li $ a ! href "#osx-homebrewcask" $ do
                    img ! alt "Homebrew logo" ! class_ "logo" ! hl_src "platform/img/distro-homebrew.png"
                    "Homebrew"
        H.div ! class_ "content" $ do
            H.div ! A.id "osx-none" ! class_ "flavor active" $ do
                p $ do
                    "The latest version of the Haskell Platform for Mac OS X is "
                    strong "8.0.1"
                    ". Note that the Haskell Platform is only compatible with "
                    strong "OS X 10.6 and later"
                    "."
                p "These packages are for Mac OS X systems not using a package manager. If you would rather install with MacPorts or Homebrew then select the appropriate option to the right. (Note that those distributions may lag behind official platform installers)."
                p "To get started perform these steps,"
                ol ! class_ "install-steps" $ do
                    li $ do
                        H.div ! class_ "step-number" $ "1"
                        H.div ! class_ "step-body" $ do
                            "Download the installer disk image,"
                            p mempty
                            H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/Haskell%20Platform%208.0.1%20Minimal%2064bit-signed-a.pkg" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                                i ! class_ "fa fa-download" $ mempty
                                "Download Minimal (64 bit)"
                            H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/Haskell%20Platform%208.0.1%20Full%2064bit-signed-a.pkg " ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                                i ! class_ "fa fa-download" $ mempty
                                "Download Full (64 bit)"
                            H.div ! class_ "download-hash" $ do
                                "You can verify the authenticity of this file by checking its "
                                strong "SHA-256"
                                "hash,"
                                ul ! class_ "hashes" $ do
                                    li $ do
                                        "64 bit Minimal:"
                                        input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "c96fb07439a6ca10d64d36200a61e4ec51a3d0b64b9ad1da40f007cd0d7fb7c6"
                                    li $ do
                                        "64 bit Full:"
                                        input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "f579f8f120998faba6a9158be7b6c218f73ce65bd041046f0a2677b8cc614129"
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
                    "To install Haskell Platform with"
                    a ! href "https://trac.macports.org/browser/trunk/dports/devel/haskell-platform/Portfile" $ "MacPorts"
                    ", simply run,"
                pre "$ sudo port install haskell-platform"
            --  #osx-macports 
            H.div ! A.id "osx-homebrewcask" ! class_ "flavor" $ do
                h3 "Homebrew Cask"
                p $ do
                    "To install Haskell Platform with"
                    a ! href "http://caskroom.io" $ "Homebrew Cask"
                    ", simply run,"
                pre "$ brew cask install haskell-platform"
            --  #osx-homebrewcask 
        H.div ! class_ "bottom-rule" $ mempty

windows_download = do
    section ! class_ "downloads-platform container" ! dataAttribute "os" "windows" ! A.id "windows" $ do
        H.div ! class_ "platform-name" $ do
            img ! hl_src "platform/img/os-windows.svg" ! alt "Windows logo"
            h2 "Windows"
        a ! class_ "expander" ! href "#windows" $ H.div $ do
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-1"
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-2"
            img ! hl_src "platform/img/expand-piece.svg" ! class_ "expand-3"
        H.div ! class_ "content" $ do
            p $ do
                "The latest version of the Haskell Platform for Windows is "
                strong "8.0.1"
                "."
            p "To get started perform these steps,"
            ol ! class_ "install-steps" $ do
                li $ do
                    H.div ! class_ "step-number" $ "1"
                    H.div ! class_ "step-body" $ do
                        p "Download the installer,"
                        H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/HaskellPlatform-8.0.1-minimal-i386-setup-a.exe" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                            i ! class_ "fa fa-download" $ mempty
                            "Download Minimal (32 bit)"
                        H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/HaskellPlatform-8.0.1-full-i386-setup-a.exe" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                            i ! class_ "fa fa-download" $ mempty
                            "Download Full (32 bit)"
                        H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/HaskellPlatform-8.0.1-minimal-x86_64-setup-a.exe" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                            i ! class_ "fa fa-download" $ mempty
                            "Download Minimal (64 bit)"
                        H.div ! class_ "download-btn" $ a ! href "//haskell.org/platform/download/8.0.1/HaskellPlatform-8.0.1-full-x86_64-setup-a.exe" ! onclick "return dl(this)" ! class_ "btn btn-haskell" $ do
                            i ! class_ "fa fa-download" $ mempty
                            "Download Full (64 bit)"
                        H.div $ do
                            "You can verify the authenticity of this file by checking its "
                            strong "SHA-256"
                            "hash,"
                            ul ! class_ "hashes" $ do
                                li $ do
                                    "32 bit Minimal:"
                                    input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "9ea9d033520d76dfd281e0bdc36625203f4003d8761ad83f599812969fe2a1ee"
                                li $ do
                                    "32 bit Full:"
                                    input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "e17941f42c44ea0f8fe478fedb47861b1970b8aa49b33d10d011b9e6a0e8592a"
                                li $ do
                                    "64 bit Minimal:"
                                    input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "8478164015715fb6ac409504316b2d01fa47bcc3c1e489092d3d23e6265c3369"
                                li $ do
                                    "64 bit Full:"
                                    input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value "b3a5a1e95e6f9348e0f02aef928c893efaa1811914c486ceb8d6898e1a2c00ce"
                li $ do
                    H.div ! class_ "step-number" $ "2"
                    H.div ! class_ "step-body" $ "Run the installer and follow the instructions."
                li $ do
                    H.div ! class_ "step-number" $ "3"
                    H.div ! class_ "step-body" $ do
                        "Modify your cabal config file (you can verify the location by running \"cabal\n           user-config init\") to contain the following lines:"
                        pre "extra-prog-path: C:\\Program Files\\Haskell Platform\\8.0.1\\msys\\usr\\bin\\nextra-lib-dirs: C:\\Program Files\\Haskell Platform\\8.0.1\\mingw\\lib\\nextra-include-dirs: C:\\Program Files\\Haskell Platform\\8.0.1\\mingw\\include"
                li $ do
                    H.div ! class_ "step-number" $ "4"
                    H.div ! class_ "step-body" $ "Start WinGHCi from the Start menu and have fun!"
        H.div ! class_ "bottom-rule" $ mempty

