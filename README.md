# shoebox-ui-halogen

[![Build Status](https://travis-ci.org/frankfurt-haskell-user-group/shoebox-ui-halogen.svg?branch=master)](https://travis-ci.org/frankfurt-haskell-user-group/shoebox-ui-halogen)

This is a UI for the shoebox project using the PureScript Halogen framework.

# Usage

1. Start the [shoebox backend](https://github.com/frankfurt-haskell-user-group/shoebox).
2. Clone and build shoebox-ui-halogen.
3. Open dist/index.html in a browser.

# Build

1. Ensure that [Node.js](https://nodejs.org/) version 4 or higher is installed:

       node --version
2. Install the Node modules:

       npm update
3. Install [Bower](https://bower.io/):

       npm install -g bower
4. Install the Bower components:

       bower update
5. Build the UI:

       npm run build
