local stepVolumes = [
  { name: 'nixstore', path: '/nix' },
  { name: 'gcroots', path: '/var/lib/drone/nix-build' },
  { name: 'nixconf', path: '/etc/nix' },
  { name: 'sslcerts', path: '/etc/ssl' },
];

local dockerVolumes = [{
  name: 'nixstore',
  host: { path: '/nix' },
}, {
  name: 'gcroots',
  host: { path: '/var/lib/drone/nix-build' },
}, {
  name: 'nixconf',
  host: { path: '/nix/var/nix/profiles/system/etc/nix' },
}, {
  name: 'sslcerts',
  host: { path: '/nix/var/nix/profiles/system/etc/ssl' },
}];

local environment = {
  NIX_REMOTE: 'daemon',
  PATH: '/nix/var/nix/profiles/system/sw/bin/',
  PAGER: 'cat',
  USER: 'root',
  BUILDDIR: '/var/lib/drone/nix-build',
};

local build = {
  name: 'Build NixOS and home-manager',
  kind: 'pipeline',
  type: 'docker',
  volumes: dockerVolumes,
  steps: [{
    name: 'build',
    image: 'busybox',
    commands: [
      // TODO fix this NIX_PATH impurities in home-manager
      |||
        export NIX_PATH=$(nix eval --raw --impure --expr '"nixpkgs=$${(builtins.getFlake (toString ./.)).inputs.nixpkgs}"')
      |||,
      'rm -rf $BUILDDIR/gcroots.tmp && mkdir -p $BUILDDIR/gcroots.tmp',
      "nix shell 'nixpkgs#git' -c nix build -L --out-link $BUILDDIR/gcroots.tmp/result -f ./nixos/ci.nix",
      'rm -rf $BUILDDIR/gcroots && mv $BUILDDIR/gcroots.tmp $BUILDDIR/gcroots',
    ],
    volumes: stepVolumes,
    environment: environment,
  }, {
    name: 'upload',
    image: 'busybox',
    commands: [
      'nix path-info --json -r $BUILDDIR/gcroots/result* > $BUILDDIR/path-info.json',
      "nix shell '.#jq' -c jq -r 'map(select(.ca == null and .signatures == null)) | map(.path) | .[]' < $BUILDDIR/path-info.json > paths",
      "nix shell '.#cachix' -c cachix push --jobs 32 mic92 < paths",
    ],
    environment: environment + {
      CACHIX_SIGNING_KEY: {
        from_secret: 'CACHIX_SIGNING_KEY',
      },
    },
    volumes: stepVolumes,
    when: {
      status: ['failure', 'success'],
    },
  }],
  trigger: {
    event: {
      exclude: ['promote', 'rollback'],
    },
  },
};

local deploy = {
  name: 'Deploy NixOS',
  kind: 'pipeline',
  type: 'docker',
  volumes: dockerVolumes,
  steps: [{
    name: 'deploy',
    image: 'busybox',
    commands: [
      |||
        mkdir -m700 -p $HOME/.ssh && echo "$DEPLOY_SSH_KEY" > $HOME/.ssh/id_ed25519 && chmod 400 $HOME/.ssh/id_ed25519
      |||,
      'cp /nix/var/nix/profiles/system/etc/ssh/ssh_known_hosts $HOME/.ssh/known_hosts',
      |||
        nix shell '.#parallel' -c parallel --line-buffer --tagstring '{}' -q nix run '.#deploy.{1}' ::: eve turingmachine eva
      |||,
    ],
    volumes: stepVolumes,
    environment: environment + {
      DEPLOY_SSH_KEY: { from_secret: 'DEPLOY_SSH_KEY' },
    },
  }],
  trigger: { event: ['promote', 'rollback'] },
};

[
  build,
  deploy
]
