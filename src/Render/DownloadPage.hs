{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for Download page

module Render.DownloadPage where

import Control.Monad
import qualified Render.Base as RB
import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base
import Render.Linux
import Render.OSX
import Render.Windows
import Render.HaskellOrg hiding (hl_src)
import HtmlDoc

download_page :: [FileInfo] -> Html
download_page files = do
  let linux_bins = binsFor OsLinux files
      sources = srcDists files
      osx_bins = binsFor OsOSX files
      windows_bins = binsFor OsWindows files
  H.head $ do
    hl_head
    H.title "Download the Haskell Platform"
    hp_head
  body ! class_ "page-home" $ do
    H.div ! class_ "wrap" $ do
      navbar_section
      -- the banner area
      H.div ! class_ "pattern-bg" $ do
        H.div ! class_ "container" $ do
          H.div ! class_ "row" $ do
            H.div ! class_ "span6 col-sm-6" $ do
              banner_left
            H.div ! class_ "span6 col-sm-6" $ do
              banner_right
      -- getting started
      H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
          H.div ! class_ "span12 col-sm-12" $ do
            getting_started
            found_user_platform
            unknown_user_platform
            platform_toc
            H.div ! class_ "container" $ do
              linux_download linux_bins sources
              H.div ! class_ "bottom-rule" $ mempty
              osx_download osx_bins
              H.div ! class_ "bottom-rule" $ mempty
              windows_download windows_bins
    hl_footer  -- same level as the "wrap" class div

-- wrap some HTML in a Download Page
download_page_wrapper :: Html -> Html
download_page_wrapper content = do
  H.head $ do
    hl_head
    H.title "Download the Haskell Platform"
    hp_head
  body ! class_ "page-home" $ do
    H.div ! class_ "wrap" $ do
      navbar_section
      -- the banner area
      H.div ! class_ "pattern-bg" $ do
        H.div ! class_ "container" $ do
          H.div ! class_ "row" $ do
            H.div ! class_ "span6 col-sm-6" $ do
              banner_left
            H.div ! class_ "span6 col-sm-6" $ do
              banner_right
      -- getting started
      H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
          H.div ! class_ "span12 col-sm-12" $ do
            getting_started
            found_user_platform
            unknown_user_platform
            platform_toc
            H.div ! class_ "container" $ 
              content
    hl_footer  -- same level as the "wrap" class div

all_sections:: [FileInfo] -> (Html,Html,Html)
all_sections files = 
  let linux_bins = binsFor OsLinux files
      sources = srcDists files
      osx_bins = binsFor OsOSX files
      windows_bins = binsFor OsWindows files

      linux   = linux_download linux_bins sources
      windows = windows_download windows_bins
      osx     = osx_download osx_bins
  in (linux, windows, osx)

download_page_for_linux :: [FileInfo] -> Html
download_page_for_linux files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper $ do
      linux
      section_hrule
      osx
      section_hrule
      windows

download_page_for_osx :: [FileInfo] -> Html
download_page_for_osx files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper $ do
      osx
      section_hrule
      windows
      section_hrule
      linux

download_page_for_windows :: [FileInfo] -> Html
download_page_for_windows files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper $ do
      windows
      section_hrule
      linux
      section_hrule
      osx

-- doc versions of download_page_for_...

download_page_wrapper' :: HtmlDoc -> Html -> HtmlDoc
download_page_wrapper' doc content =
  let d1 = appendHead doc $ do
              hl_head
              H.title "Download the Haskell Platform"
              hp_head
      d2 = appendBody d1 $ do
              H.div ! class_ "wrap" $ do
                navbar_section
                -- the banner area
                H.div ! class_ "pattern-bg" $ do
                  H.div ! class_ "container" $ do
                    H.div ! class_ "row" $ do
                      H.div ! class_ "span6 col-sm-6" $ do
                        banner_left
                      H.div ! class_ "span6 col-sm-6" $ do
                        banner_right
                -- getting started
                H.div ! class_ "container" $ do
                  H.div ! class_ "row" $ do
                    H.div ! class_ "span12 col-sm-12" $ do
                      getting_started
                      found_user_platform
                      unknown_user_platform
                      platform_toc
                      H.div ! class_ "container" $ 
                        content
              hl_footer  -- same level as the "wrap" class div
  in d2

download_page_for_linux' :: HtmlDoc -> [FileInfo] -> HtmlDoc
download_page_for_linux' doc files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper' doc $ do
      linux
      section_hrule
      osx
      section_hrule
      windows

download_page_for_windows' :: HtmlDoc -> [FileInfo] -> HtmlDoc
download_page_for_windows' doc files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper' doc $ do
      windows
      section_hrule
      linux
      section_hrule
      osx

download_page_for_osx' :: HtmlDoc -> [FileInfo] -> HtmlDoc
download_page_for_osx' doc files = do
  let (linux, windows, osx) = all_sections files
  download_page_wrapper' doc $ do
      osx
      section_hrule
      windows
      section_hrule
      linux

-- -----


hp_head = do
    link ! href  "801/download.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css" ! rel "stylesheet" ! type_ "text/css"
    H.style $ do
      ".hp-branding { font-family: sans-serif; line-height: 50px; font-weight: bold; font-size: 50px; background-repeat: no-repeat; background-size: 70px; display: block; padding-left: 80px; background-position: left; } "
      ".hp-summary { margin-top: 20px; display: block; font-size: 20px; }"
    script ! hl_src "/platform/js/jquery-1.11.1.min.js" $ mempty
    script ! src "801/download.js" $ mempty

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
    h2 ! A.id "get-started" $ "Welcome to Haskell!"
    p $ do
      "The Haskell Platform 8.0.1 is available in two versions:"
    H.div ! class_ "variety-wrapper" $ do
      H.div ! class_ "variety-row" $ do
        H.div ! class_ "variety-col1" $ do
          strong "Full"
        H.div ! class_ "variety-col2" $ do
         "the Platform with the complete library of pre-compiled packages"
      H.div ! class_ "variety-row" $ do
        H.div ! class_ "variety-col1" $ do
          strong "Minimal"
        H.div ! class_ "variety-col2" $ do
         "GHC, profiling and development tools with a minimal set of library packages"

    p $ do
      "The " >> strong "Minimal" >> " version is recommended for those users who"
      " are familiar with installing packages from Hackage."
      " The " >> strong "Full"
      " version is made available for those accustomed to previous versions of the Platform which included a broader set of pre-compiled packages."

    p $ do
      "Both versions come with the "
      code $ "stack"
      " development tool. The tool can upgrade itself to the latest version with "
      code "stack upgrade"
      ". Users should also consult "
      a ! href "http://docs.haskellstack.org/en/stable/install_and_upgrade/#path" $ "Stack User's Guide"
      " for more details on configuring the PATH environment variable for use with Stack."

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
        li ! A.id "osx-select" ! class_ "platform-select" ! dataAttribute "platform" "osx" $ a ! href "#osx-section" $ do
            img ! hl_src "platform/img/os-osx.svg" ! alt "Mac OS X logo"
            "Mac OS X"
        li ! A.id "windows-select" ! class_ "platform-select" ! dataAttribute "platform" "windows" $ a ! href "#windows-section" $ do
            img ! hl_src "platform/img/os-windows.svg" ! alt "Windows logo"
            "Windows"
        li ! A.id "linux-select" ! class_ "platform-select" ! dataAttribute "platform" "linux" $ a ! href "#linux-section" $ do
            img ! hl_src "platform/img/os-linux.svg" ! alt "Linux logo"
            "Linux"

