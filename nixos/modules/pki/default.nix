{ config
, pkgs
, lib
, ...
}:
with lib; let
  certFile = config.environment.etc."ssl/certs/ca-certificates.crt".source;
in
{
  environment.etc."pki/nssdb".source =
    pkgs.runCommand "system-wide-nssdb"
      {
        inherit certFile;
        buildInputs = [
          pkgs.jq
          pkgs.nssTools
        ];
        parseInfoScript =
          /*
        jq
          */
          ''
            ${builtins.toJSON certFile} as $certFile |

            split("\t-----END CERTIFICATE-----\n")[] |
            select(test("\t-----BEGIN CERTIFICATE-----\n")) |
            . + "\t-----END CERTIFICATE-----\n" |

            sub("^([0-9]+\t\n)*";"") |

            (match("^([0-9]+)\t").captures[0].string | tonumber) as $lineNumber |

            gsub("(?m)^[0-9]+\t";"") |

            match("^([^\n]+)\n(.*)";"m").captures | map(.string) |

            # Line numbers are added to the names to ensure uniqueness.
            "\(.[0]) (\($certFile):\($lineNumber))" as $name |
            .[1] as $cert |

            { $name, $cert }
          '';
        passAsFile = [
          "parseInfoScript"
        ];
      }
      /*
        sh
      */
      ''
        mkdir nssdb

        nl -ba -w1 "$certFile" |
        jq -ceRs -f "$parseInfoScriptPath" > certinfo.ndjson

        exec < certinfo.ndjson
        while read -r certinfo; do
          name=$(printf %s "$certinfo" | jq -er .name)
          cert=$(printf %s "$certinfo" | jq -er .cert)

          printf %s "$cert" | certutil -A -d nssdb -n "$name" -t C,C,C
        done

        mv nssdb "$out"
      '';

  environment.variables = flip genAttrs (_: toString certFile) [
    "CURL_CA_BUNDLE"
    "GIT_SSL_CAINFO"
    "SSL_CERT_FILE"
  ];

  security.pki.certificateFiles =
    mapAttrsToList
      (name: const (./certs + "/${name}"))
      (filterAttrs (const (dir: "regular" == dir))
        (builtins.readDir ./certs));
}
