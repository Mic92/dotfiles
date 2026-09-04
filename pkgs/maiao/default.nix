{
  lib,
  buildGoModule,
  fetchFromGitHub,
  installShellFiles,
  stdenv,
}:

buildGoModule (finalAttrs: {
  pname = "maiao";
  version = "1.4.1";

  src = fetchFromGitHub {
    owner = "runetes";
    repo = "maiao";
    tag = "maiao-v${finalAttrs.version}";
    hash = "sha256-3pZc/wORPjIQw2VbvqrF9GjNN/IsosA1q8mSASfKlqI=";
  };

  vendorHash = "sha256-1q88bEFo1RKOE9k1Ii3ThcahECQVF40yHUVVEk08RXw=";

  subPackages = [ "cmd/maiao" ];

  ldflags = [
    "-s"
    "-w"
    "-X github.com/adevinta/maiao/pkg/version.Version=${finalAttrs.version}"
  ];

  nativeBuildInputs = [ installShellFiles ];

  # Upstream ships the binary as git-review so it works as `git review`.
  postInstall = ''
    mv $out/bin/maiao $out/bin/git-review
  ''
  + lib.optionalString (stdenv.buildPlatform.canExecute stdenv.hostPlatform) ''
    installShellCompletion --cmd git-review \
      --bash <($out/bin/git-review completion bash) \
      --fish <($out/bin/git-review completion fish) \
      --zsh <($out/bin/git-review completion zsh)
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/git-review version | grep -q "^${finalAttrs.version}$"
  '';

  meta = {
    description = "Seamless GitHub PR management from the command-line (stacked PRs)";
    homepage = "https://github.com/runetes/maiao";
    license = lib.licenses.mit;
    mainProgram = "git-review";
  };
})
