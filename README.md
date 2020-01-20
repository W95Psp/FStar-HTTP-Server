# HTTP server + F*-to-JS + ELM

This implements an minimal HTTP server as a F* tactic (yeaah), and `FStar.Tactics.JavaScript.*` gives JS definitions like `window`, what is a DOM, etc.

It also implements a F*-to-JS converter as a F* tactic.

On the top of this, I made a minimal ELM-like enigne, to make functional reactive web programming. So one can start a HTTP server whinin F* as a tactic, generate JS definition for a bunch of functions, send it to a browser, and provide the browser user a client-side web app, written in F*!

# [F* web app demo](https://raw.githack.com/W95Psp/FStar-HTTP-Server/master/index.html)
[`index.html`](https://raw.githack.com/W95Psp/FStar-HTTP-Server/master/index.html) is an example of a tiny app, its F* code lives in `example.fst` 





