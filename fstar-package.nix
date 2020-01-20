  { name = "HTTP-server";
    sources-directory = ./.;
    sources = [
    ];
    ocaml-sources = [
    ];
    # tactic-module = "FStar.Tactics.Tcp";
    dependencies =
      with (import /home/lucas/Bureau/Fstar-libs);
      [
        Data.JSON
        FStar-Tactics-JS
        (import ./FStar.Tactics.Tcp)
      ];
    compile = [];
  }
