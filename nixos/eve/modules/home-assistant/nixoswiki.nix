{
  services.home-assistant.config.feedreader = {
    urls = [
      "https://nixos.wiki/api.php?hidebots=1&days=7&limit=50&action=feedrecentchanges&feedformat=atom"
    ];
    scan_interval.minutes = 5;
    max_entries = "10";
  };
}
