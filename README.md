# haskify

Haskify is a (currently) GETful API wrapper for
the [Spotify API](https://developer.spotify.com/web-api/) written in Haskell

## Getting started

````
  stack install
````

Go to https://developer.spotify.com/my-applications/ and register an
application for testing. Add your client ID and secret to test/Secrets.hs

## Available Endpoints

Haskify supports the following endpoints:

- albums
- artists
- audio-features
- browse
- search
- tracks

Haskify uses sane types to represent parameters for endpoints.

The following endpoints remain unsupported:

- audio-analysis
- me
- recommendations
- users


## Built With

- wreq
- aeson

## Authors
- [Colin Burr](https://github.com/sw3dish)
- [John Kastner](https://github.com/jackastner)

## Acknowledgements
- Thanks to [Niki Vazou](https://github.com/nikivazou) and
[CMSC498V](https://nikivazou.github.io/CMSC498V/) for providing the impetus for
this project
