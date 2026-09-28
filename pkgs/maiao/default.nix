{
  lib,
  buildGoModule,
  fetchFromGitHub,
  installShellFiles,
  stdenv,
}:

buildGoModule (finalAttrs: {
  pname = "maiao";
  version = "1.8.0";

  src = fetchFromGitHub {
    owner = "runetes";
    repo = "maiao";
    tag = "maiao-v${finalAttrs.version}";
    hash = "sha256-SfeNjn8TGiy9ywx8ZNZeKMi5bC35prmC3k5+M9j5YGQ=";
  };

  vendorHash = "sha256-ccuTrhRrH+Qe9VwIKSK9rQYcYtrB8YO3pODbMT5/sVc=";

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
