# lkdoko

![](https://github.com/martinhh/lkdoko/workflows/test/badge.svg)

lkdoko is a simple server-/client-implementation of the popular (mainly in Germany) card game
[Doppelkopf](https://en.wikipedia.org/wiki/Doppelkopf).

The frontend is implemented in scala.js using [Laminar](https://github.com/raquo/Laminar) (and
[laminext](https://github.com/tulz-app/laminext)).

The backend's business logic is implemented as purely functional, immutable state machine and aims to be portable to
different server libraries.

There currently are two server implementations: one using [pekko-http](https://github.com/apache/incubator-pekko-http),
one using [http4s](https://github.com/http4s/http4s).


## Why?

During the spring 2020 C-19 lockdown, there was a sudden need for a solution to play this game remotely.

While there are a few commercial offerings, none of these offer the exact rules that I like to play - and none of them are free.

By now, it also serves as a personal playground for trying out new scala stuff.

## Project Status

The project still is a "minimal playable prototype".

The UI doesn't look pretty and there are various features that one could hope for that are not implemented
(e.g.: configurable rules, supporting multiple games in parallel, ...).

Nevertheless, playing it in this state has already been fun for a few test users.

## Acknowledgements

This project took inspiration from the following projects:

* [jrudolph/pekko-http-scala-js-websocket-chat](https://github.com/jrudolph/pekko-http-scala-js-websocket-chat) for the
  (intial) overall project structure

Svg playing cards were copied from [vector-playing-cards](https://code.google.com/archive/p/vector-playing-cards/).