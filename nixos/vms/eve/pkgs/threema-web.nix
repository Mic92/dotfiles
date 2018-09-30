{ fetchzip }:
let 
  version = "2.0.3";
in fetchzip {
  url = "https://github.com/threema-ch/threema-web/releases/download/v${version}/threema-web-${version}-gh.tar.gz";
  sha256 = "15yr80p88dl92bzpfmpnyc9waiaccg6vkby2vlvmzfrgbm3rza20";
}
