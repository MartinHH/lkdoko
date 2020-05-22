# lkdoko

![](https://github.com/martinhh/lkdoko/workflows/test/badge.svg)

lkdoko is a simple server-/client-implementation of the popular (mainly in Germany) card game [Doppelkopf](https://en.wikipedia.org/wiki/Doppelkopf).

It uses akka-http for the server and scala.js for the client.

## Why?

During the recent (spring 2020) C-19 lockdown, there was a sudden need for a solution to play this game remotely.

While there are a few commercial offerings, none of these offer the exact rules that I like to play - and none of them are free.

Furthermore, I had intended to try out scala.js in a cross-compiled project for a while now, so this was a good occasion to do it.

## Project Status

This project currently is a "minimal playable prototype".

There are still a few limitations in the game itself, the client doesn't look pretty and parts of the code (especially the frontend) leave a lot of room for improvement.

Nevertheless, playing it in this state has already been fun for a few test users.

## Acknowledgements

This project took inspiration from the following projects:

* [jrudolph/akka-http-scala-js-websocket-chat](https://github.com/jrudolph/akka-http-scala-js-websocket-chat) for the overall project structure
* [rleibman/scalajs-reconnecting-websocket](https://github.com/rleibman/scalajs-reconnecting-websocket) for the client's reconnect logic
