build = {
  "name": 'Build NixOS and home-manager',
  "kind": 'pipeline',
  "type": 'exec',
  "clone": {  "depth": 1 },
  "steps": [
  {
    "name": 'build',
    "commands": [
      "mkdir gcroots",
      "nix run --override-input nixpkgs github:Mic92/nixpkgs github:Mic92/drone-nix-scheduler#hydra-eval-jobs -- --workers 4 --gc-roots-dir $PWD/gcroots --flake .# > eval.json",
      "nix run --override-input nixpkgs github:Mic92/nixpkgs github:Mic92/drone-nix-scheduler -- eval.json"
    ],
    "environment": {
      "DRONE_SERVER": "https://drone.thalheim.io",
      "DRONE_TOKEN": {"from_secret": 'DRONE_TOKEN'},
    },
  }, {
    "name": 'upload',
    "commands": [
      "nix path-info --json -r $PWD/gcroots/*.drv > path-info.json",
      # only local built derivations
      "nix shell 'nixpkgs#jq' -c jq -r 'map(select(.ca == null and .signatures == null)) | map(.path) | .[]' < path-info.json > paths",
      "nix shell 'nixpkgs#cachix' -c cachix push --jobs 32 mic92 < paths",
    ],
    "environment": {
      "CACHIX_SIGNING_KEY": { "from_secret": 'CACHIX_SIGNING_KEY', }
    },
    "when": {
      "event": { "exclude": ['pull_request'] },
      "status": ['failure', 'success'],
    },
  }, {
    "name": 'send irc notification',
    "commands": [
      'LOGNAME=drone nix run github:Mic92/nur-packages#irc-announce -- irc.r 6667 drone "#xxx" "build $DRONE_SYSTEM_PROTO://$DRONE_SYSTEM_HOST/$DRONE_REPO/$DRONE_BUILD_NUMBER : $DRONE_BUILD_STATUS" || true'
    ],
    "when": {
      "status": ['failure', 'success'],
    },
  }],
  "trigger": {
    "branch": 'master',
    "event": [ "push", "pull_request"],
  },
};

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
    build,
    buildExpression,
    deploy('eve'),
    deploy('turingmachine'),
    deploy('eva'),
    deploy('rock'),
    deploy('joerg@turingmachine'),
    #deploy('eddie'),
    #deploy('joerg@eddie'),
    deploy('joerg@eve'),
  ]
