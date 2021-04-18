def environment(extra={}):
  e = { "BUILDDIR": '/var/lib/drone/nix-build' }
  e.update(extra)
  return e

buildCi = 'nix shell nixpkgs#git -c nix build -L --option keep-going true --out-link $BUILDDIR/gcroots.tmp/result -f ./nixos/ci.nix'

eval = {
  "name": 'Eval jobsets',
  "kind": 'pipeline',
  "type": 'exec',
  "nix-jobset": True,
  "clone": {  "depth": 1 },
  "steps": [{
    "name": 'build',
    "commands": [
      'rm -rf $BUILDDIR/gcroots.tmp && mkdir -p $BUILDDIR/gcroots.tmp',
      'echo <hydra-eval-jobs>',
      'nix run --no-write-lock-file --override-input nixpkgs github:Mic92/nixpkgs https://github.com/Mic92/hydra-eval-jobs github:Mic92/drone-nix-scheduler#hydra-eval-jobs -- --workers 4 --gc-roots-dir $PWD/gcroots --flake .#',
      'echo </hydra-eval-jobs>',
      'rm -rf $BUILDDIR/gcroots && mv $BUILDDIR/gcroots.tmp $BUILDDIR/gcroots',
    ],
    "environment": environment(),
  }],
  "trigger": {
    "branch": 'master',
    "event": [ "push", "pull_request"],
  },
}

build = {
  "kind": 'pipeline',
  "name": 'Build nix derivation',
  "type": 'exec',
  "nix-build": True,
  "clone": {  "depth": 1 },
  "steps": [{
    "name": 'build',
    "commands": [
      'nix build -L $derivation'
    ],
    "environment": environment(),
  }],
  "trigger": {
    "branch": 'master',
    "event": [ "push", "pull_request"],
  },
};

notify = {
  "steps": [{
    "name": 'send irc notification',
    "commands": [
      'LOGNAME=drone nix run github:Mic92/nur-packages#irc-announce -- irc.r 6667 drone "#xxx" "build $DRONE_SYSTEM_PROTO://$DRONE_SYSTEM_HOST/$DRONE_REPO/$DRONE_BUILD_NUMBER : $DRONE_BUILD_STATUS" || true'
    ],
    "when": {
      "status": ['failure', 'success'],
      "event": { "exclude": ['pull_request'] },
    },
  }],
  "nix-post-build": True,
  "trigger": {
    "branch": 'master',
    "event": [ "push", "pull_request"],
  },
}

buildExpression = {
  "name": 'Build nix derivation',
  "kind": 'pipeline',
  "type": 'exec',
  "steps": [{
    "name": 'build',
    "commands": [
      'echo $derivation',
      'nix build -L $derivation'
    ],
  }],
  "trigger": { "event": ['custom'] },
};

def deploy(target):
  return {
    "name": 'Deploy to ' + target,
    "kind": 'pipeline',
    "type": 'exec',
    "steps": [{
      "name": 'deploy',
      "commands": [
        'eval $(nix shell nixpkgs#openssh -c ssh-agent) && ' +
        'echo "$DEPLOY_SSH_KEY" | nix shell nixpkgs#openssh -c ssh-add - && ' +
        'nix run .#deploy.%s' % target,
       ],
       "environment": {
         "DEPLOY_SSH_KEY": { "from_secret": 'DEPLOY_SSH_KEY' },
       },
      }],
      "trigger": { "event": ['promote', 'rollback'] },
  }

def main(ctx):
  return [
    eval,
    build,
    #buildExpression,
    deploy('eve'),
    deploy('turingmachine'),
    deploy('eva'),
    deploy('rock'),
    deploy('joerg@turingmachine'),
    #deploy('eddie'),
    #deploy('joerg@eddie'),
    deploy('joerg@eve'),
  ]
