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
    privileged: true,
    commands: [
      'rm -rf $BUILDDIR/gcroots.tmp && mkdir -p $BUILDDIR/gcroots.tmp',
      'ulimit -n',
      'ulimit -n 40096',
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
      # only local built derivations
      "nix shell 'nixpkgs#jq' -c jq -r 'map(select(.ca == null and .signatures == null)) | map(.path) | .[]' < $BUILDDIR/path-info.json > paths",
      "nix shell 'nixpkgs#cachix' -c cachix push --jobs 32 mic92 < paths",
    ],
    environment: environment {
      CACHIX_SIGNING_KEY: {
        from_secret: 'CACHIX_SIGNING_KEY',
      },
    },
    volumes: stepVolumes,
    when: {
      event: { exclude: ['pull_request'] },
      status: ['failure', 'success'],
    },
  }],
  trigger: {
    event: {
      exclude: ['promote', 'rollback'],
    },
  },
};

local deploy(target) = {
  name: 'Deploy to ' + target,
  kind: 'pipeline',
  type: 'docker',
  volumes: dockerVolumes,
  steps: [{
    name: 'deploy',
    image: 'busybox',
    commands: [
      'install -D /nix/var/nix/profiles/system/etc/ssh/ssh_known_hosts $HOME/.ssh/known_hosts',
      'echo "Host eve.thalheim.io\nForwardAgent yes" > $HOME/.ssh/config',
      'eval $(ssh-agent) && ' +
      'echo "$DEPLOY_SSH_KEY" | ssh-add - && ' +
      'nix run .#deploy.%s' % target,
    ],
    volumes: stepVolumes,
    environment: environment {
      DEPLOY_SSH_KEY: { from_secret: 'DEPLOY_SSH_KEY' },
    },
  }],
  trigger: { event: ['promote', 'rollback'] },
};

[
  build,
  deploy('eve'),
  deploy('turingmachine'),
  deploy('eva'),
  deploy('eddie'),
  deploy('joerg@turingmachine'),
  deploy('joerg@eddie'),
  deploy('joerg@eve'),
]
