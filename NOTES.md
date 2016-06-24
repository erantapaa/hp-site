
## General Notes

### Site Struture

Here's the layout of the site and the routines which generate them:

    .
    ├── 801
    │   ├── contents.js
    │   ├── download.css
    │   ├── download.js
    │   └── logo.png
    ├── contents.html         included_packages_page
    ├── index.html            (copy of windows.html)
    ├── linux.html            download_page_for_linux
    ├── osx.html              download_page_for_osx
    ├── prior.html            prior_releases_page
    └── windows.html          download_page_for_windows
    └── faq.html              faq_page

### Page Structure

General page struture:

    <head>
      ... header items from haskell.org ...
      ... platform specific headers ...
    </head>
    <body>
      <div class="wrap">
        <nav> ... </nav>
        <div class="pattern-bg"> ... </div>   <!-- HP banner area -->
        <div class="container"> ... </div>    <!-- page body -->
      </div>
      <div class="footer">
        ... footer from haskell.org ...
      </div>
      ... end scripts ...
    </body>

### Asset files

The directory `assets` contains asset files (css, js, pngs, etc.) specfic
for this release. They will be deployed to a release-specific directory
so as not to conflict with prior releases.

To add an asset file:

1. Copy the file to the `assets` directory
2. Add the file name to the `data-files:` section of the .cabal file.
3. In `buildAllPages`, add a call to `copyDataFile` to copy the asset file
to the build directory.

To reference an asset file in a page, use the `asset` function defined in
`Render.Base` to create a relative url:

    asset :: String -> AttributeValue

When creating a new release, simply change the value of `asset_dir`
to ensure that assets for the new release don't conflict with
assets for previous releases.

### Testing with JS disabled

To test a Download Page with JS disabled, simply
rename the file to include `'-nojs` in the file name, e.g.:

   cp linux.html linux-nojs.html
   open linux-nojs.html

