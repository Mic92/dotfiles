{
  # Former GitHub Pages blog URL; keep inbound links working.
  services.nginx.virtualHosts."writey-things.lekwati.com" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    globalRedirect = "blog.lekwati.com";
  };
}
