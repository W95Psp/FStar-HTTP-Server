  { name = "FStar.Tactics.Tcp";
    sources-directory = ./.;
    sources = [
      "FStar.Tactics.Tcp"
      "FStar.Tactics.TcpI.fsti"
    ];
    ocaml-sources = [
      "FStar_Tactics_TcpI.ml"
    ];
    tactic-module = "FStar.Tactics.Tcp";
    dependencies =
      with (import /home/lucas/Bureau/Fstar-libs);
      [ 
      ];
    compile = [];
  }
