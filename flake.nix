{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-generators = {
      #url = "github:nix-community/nixos-generators";
      url = "github:Mic92/nixos-generators/fedf7136f27490402fe8ab93e67fafae80513e9b";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";

    mars-std.url = "github:mars-research/mars-std";
    mars-std.inputs.nixpkgs.follows = "nixpkgs";
    mars-std.inputs.rust-overlay.follows = "rust-overlay";
    mars-std.inputs.flake-utils.follows = "flake-utils";

    miniond.url = "github:mars-research/miniond";
    miniond.inputs.mars-std.follows = "mars-std";

    vmsh.url = "github:Mic92/vmsh";
    vmsh.inputs.flake-utils.follows = "flake-utils";
    vmsh.inputs.nixpkgs.follows = "nixpkgs";
    vmsh.inputs.fenix.follows = "fenix";

    nixpkgs.url = "github:Mic92/nixpkgs/main";
    nixpkgs-systemd.url = "github:andir/nixpkgs/systemd-250";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur.url = "github:nix-community/NUR";
    # for development
    #sops-nix.url = "/home/joerg/git/sops-nix";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    bme680-mqtt.url = "github:Mic92/bme680-mqtt";
    bme680-mqtt.inputs.utils.follows = "flake-utils";
    bme680-mqtt.inputs.nixpkgs.follows = "nixpkgs";

    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";
    nix-ld.inputs.utils.follows = "flake-utils";

    envfs.url = "github:Mic92/envfs";
    envfs.inputs.nixpkgs.follows = "nixpkgs";
    envfs.inputs.utils.follows = "flake-utils";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { ... } @ args: import ./outputs.nix args;
}
