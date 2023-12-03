{ pkgs
, lib
, config
, ...
}:
let
  virtualRegex = pkgs.writeText "virtual-regex" ''
    /^joerg(\.|\+)[^@.]+@thalheim\.io$/ joerg@thalheim.io
    /^shannan(\.|\+)[^@.]+@lekwati\.com/ shannan@lekwati.com
    /^albert-[^@.]+@halfco\.de$/ albert@halfco.de
    /^devkid-[^@.]+@devkid\.net$/ devkid@devkid.net
    /^ls1-logins-[^@.]+@thalheim.io$/ ls1-logins@lists.lrz.de
    /^info\.[^@.]+@davhau\.com$/ info@davhau.com
    /^dave\.[^@.]+@davhau\.com$/ info@davhau.com
  '';

  domains = pkgs.writeText "domains.cf" ''
    server_host = ldap://127.0.0.1
    search_base = dc=domains,dc=mail,dc=eve
    query_filter = (&(dc=%s)(objectClass=mailDomain))
    result_attribute = postfixTransport
    bind = no
    scope = one
  '';

  accountsmap = pkgs.writeText "accountsmap.cf" ''
    server_host = ldap://127.0.0.1
    search_base = ou=users,dc=eve
    query_filter = (&(objectClass=mailAccount)(mail=%s))
    result_attribute = mail
    bind = no
  '';

  aliases = pkgs.writeText "aliases.cf" ''
    server_host = ldap://127.0.0.1
    search_base = dc=aliases,dc=mail,dc=eve
    query_filter = (&(objectClass=mailAlias)(mail=%s))
    result_attribute = maildrop
    bind = no
  '';

  helo_access = pkgs.writeText "helo_access" ''
    ${config.networking.eve.ipv4.address}   REJECT Get lost - you're lying about who you are
    ${config.networking.eve.ipv6.address}   REJECT Get lost - you're lying about who you are
    thalheim.io   REJECT Get lost - you're lying about who you are
    lekwati.com   REJECT Get lost - you're lying about who you are
  '';
  enableRblOverride = false;
  rbl_override = pkgs.writeText "rbl_override" ''
    # pfpleisure.org
    95.141.161.114 OK
  '';
in
{
  services.postfix = {
    enable = true;
    enableSubmission = true;
    hostname = "mail.thalheim.io";
    domain = "thalheim.io";

    masterConfig."465" = {
      type = "inet";
      private = false;
      command = "smtpd";
      args = [
        "-o smtpd_client_restrictions=permit_sasl_authenticated,reject"
        "-o syslog_name=postfix/smtps"
        "-o smtpd_tls_wrappermode=yes"
        "-o smtpd_sasl_auth_enable=yes"
        "-o smtpd_tls_security_level=none"
        "-o smtpd_reject_unlisted_recipient=no"
        "-o smtpd_recipient_restrictions="
        "-o smtpd_relay_restrictions=permit_sasl_authenticated,reject"
        "-o milter_macro_daemon_name=ORIGINATING"
      ];
    };

    mapFiles."virtual-regex" = virtualRegex;
    mapFiles."helo_access" = helo_access;
    mapFiles."rbl_override" = rbl_override;

    config = {
      smtp_bind_address = config.networking.eve.ipv4.address;
      smtp_bind_address6 = "2a01:4f9:2b:1605::1";
      mailbox_transport = "lmtp:unix:private/dovecot-lmtp";
      masquerade_domains = "ldap:${domains}";
      virtual_mailbox_domains = "ldap:${domains}";
      virtual_alias_maps = "ldap:${accountsmap},ldap:${aliases},regexp:/var/lib/postfix/conf/virtual-regex";
      virtual_transport = "lmtp:unix:private/dovecot-lmtp";

      # bigger attachement size
      mailbox_size_limit = "202400000";
      message_size_limit = "51200000";
      smtpd_helo_required = "yes";
      smtpd_delay_reject = "yes";
      strict_rfc821_envelopes = "yes";

      # send Limit
      smtpd_error_sleep_time = "1s";
      smtpd_soft_error_limit = "10";
      smtpd_hard_error_limit = "20";

      smtpd_use_tls = "yes";
      smtp_tls_note_starttls_offer = "yes";
      smtpd_tls_security_level = "may";
      smtpd_tls_auth_only = "yes";

      smtp_dns_support_level = "dnssec";
      smtp_tls_security_level = "dane";

      smtpd_tls_cert_file = "/var/lib/acme/mail.thalheim.io/full.pem";
      smtpd_tls_key_file = "/var/lib/acme/mail.thalheim.io/key.pem";
      smtpd_tls_CAfile = "/var/lib/acme/mail.thalheim.io/fullchain.pem";

      smtpd_tls_dh512_param_file = config.security.dhparams.params.postfix512.path;
      smtpd_tls_dh1024_param_file = config.security.dhparams.params.postfix2048.path;

      smtpd_tls_session_cache_database = ''btree:''${data_directory}/smtpd_scache'';
      smtpd_tls_mandatory_protocols = "!SSLv2,!SSLv3,!TLSv1,!TLSv1.1";
      smtpd_tls_protocols = "!SSLv2,!SSLv3,!TLSv1,!TLSv1.1";
      smtpd_tls_mandatory_ciphers = "medium";
      tls_medium_cipherlist = "AES128+EECDH:AES128+EDH";

      # authentication
      smtpd_sasl_auth_enable = "yes";
      smtpd_sasl_local_domain = "$mydomain";
      smtpd_sasl_security_options = "noanonymous";
      smtpd_sasl_tls_security_options = "$smtpd_sasl_security_options";
      smtpd_sasl_type = "dovecot";
      smtpd_sasl_path = "/var/lib/postfix/queue/private/auth";
      smtpd_relay_restrictions = "permit_mynetworks,
                                 permit_sasl_authenticated,
                                 ${lib.optionalString enableRblOverride "check_client_access hash:/etc/postfix/rbl_override,"}
                                 defer_unauth_destination";
      smtpd_client_restrictions = "permit_mynetworks,
                                permit_sasl_authenticated,
                                 ${lib.optionalString enableRblOverride "check_client_access hash:/etc/postfix/rbl_override,"}
                                reject_invalid_hostname,
                                reject_unknown_client,
                                permit";
      smtpd_helo_restrictions = "permit_mynetworks,
                              permit_sasl_authenticated,
                              ${lib.optionalString enableRblOverride "check_client_access hash:/etc/postfix/rbl_override,"}
                              reject_unauth_pipelining,
                              reject_non_fqdn_hostname,
                              reject_invalid_hostname,
                              warn_if_reject reject_unknown_hostname,
                              permit";
      smtpd_recipient_restrictions = "permit_mynetworks,
                               ${lib.optionalString enableRblOverride "check_client_access hash:/etc/postfix/rbl_override,"}
                               permit_sasl_authenticated,
                               reject_non_fqdn_sender,
                               reject_non_fqdn_recipient,
                               reject_non_fqdn_hostname,
                               reject_invalid_hostname,
                               reject_unknown_sender_domain,
                               reject_unknown_recipient_domain,
                               reject_unknown_client_hostname,
                               reject_unauth_pipelining,
                               reject_unknown_client,
                               permit";
      smtpd_sender_restrictions = "permit_mynetworks,
                          permit_sasl_authenticated,
                          ${lib.optionalString enableRblOverride "check_client_access hash:/etc/postfix/rbl_override,"}
                          reject_non_fqdn_sender,
                          reject_unknown_sender_domain,
                          reject_unknown_client_hostname,
                          reject_unknown_address";

      smtpd_etrn_restrictions = "permit_mynetworks, reject";
      smtpd_data_restrictions = "reject_unauth_pipelining, reject_multi_recipient_bounce, permit";
    };
  };

  security.dhparams = {
    enable = true;
    params.postfix512.bits = 512;
    params.postfix2048.bits = 1024;
  };

  security.acme.certs."mail.thalheim.io" = {
    postRun = "systemctl restart postfix.service";
    group = "postfix";
    dnsProvider = "rfc2136";
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
  };

  networking.firewall.allowedTCPPorts = [
    25 # smtp
    465 # stmps
    587 # submission
  ];
}
