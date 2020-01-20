# HTTP server + F*-to-JS + ELM

This implements an minimal HTTP server as a F* tactic (yeaah), and `FStar.Tactics.JavaScript.*` gives JS definitions like `window`, what is a DOM, etc.

It also implements a F*-to-JS converter as a F* tactic.

On the top of this, I made a minimal ELM-like enigne, to make functional reactive web programming. So one can start a HTTP server whinin F* as a tactic, generate JS definition for a bunch of functions, send it to a browser, and provide the browser user a client-side web app, written in F*!

# [F* web app demo](https://rawcdn.githack.com/W95Psp/FStar-HTTP-Server/5269e1cc04d94cf2085be3f11bf9316595c802c6/index.html)
[`index.html`](https://rawcdn.githack.com/W95Psp/FStar-HTTP-Server/5269e1cc04d94cf2085be3f11bf9316595c802c6/index.html) is an example of a tiny app, its F* code lives in `example.fst` 





