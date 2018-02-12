{ pkgs }:
{
  deployment.keys."monit-email.cfg".keyFile = ../secrets/monit-email.cfg;

  systemd.services.nixops-keys.postStart = ''
    ${pkgs.coreutils}/bin/install -m400 -D /run/keys/monit-email.cfg /var/lib/monit/monit-email.cfg
  '';

  services.monit = {
    enable = true;
    config = ''
      set daemon 60
      set alert uxvxdokw8k@pomail.net mail-format { from: Monit <jthalheim@gmail.com> }
      set alert joerg@thalheim.io mail-format { from: Monit <jthalheim@gmail.com> }
      set ssl options { verify: enable }
  
      include /var/lib/monit/*.cfg
  
      check host eve.higgsboson.tk with address "eve.higgsboson.tk"
        if failed 
            ping count 5 size 128 
            with timeout 5 seconds 
        then alert
        if failed 
            port 22022 
            protocol ssh 
        then alert
      
        ## http
        if failed
            host rss.devkid.net
            port 443
            #address rss.devkid.net
            timeout 10 
            seconds for 20 cycles 
        then alert
      
        if failed 
            host grafana.thalheim.io 
            port 443 
            protocol https 
            timeout 10 
            seconds for 20 cycles 
        then alert
      
        if failed
            host git.higgsboson.tk
            port 443
            protocol https
        method get
            timeout 10
            seconds for 20 cycles
        then alert
      
        ## mail
        if failed 
            host smtp.thalheim.io
            port 587 
            protocol smtp 
        then alert
        if failed 
            host imap.thalheim.io
            port 143 
            type tcp
            # imap greeting not detected
            #protocol imap 
        then alert
      
        ## xmpp
        if failed 
            host jabber.thalheim.io
            port 5222 
        then alert
        if failed 
            host jabber.thalheim.io
            port 5269 
        then alert
      
        ## dns
        if failed 
            host ns.thalheim.io
            port 53 
            use type udp 
            protocol dns 
        then alert
      '';
  };
}
