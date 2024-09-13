{
  pamixer,
  fuzzel,
  pulseaudio,
  runCommand,
  lib,
  runtimeShell,
  bluez,
  libnotify,
}:
runCommand "audio-chooser" { } ''
  mkdir -p $out/bin $out/share/applications

  ${lib.concatMapStringsSep "\n"
    (chooser: ''
      cat > $out/bin/${chooser} <<'EOF'
      #!${runtimeShell}
      export PATH=${
        lib.makeBinPath [
          pamixer
          fuzzel
          pulseaudio
          bluez
          libnotify
        ]
      }:$PATH
      ${builtins.readFile (./. + "/${chooser}.sh")}
      EOF
      chmod +x $out/bin/${chooser}
      cat > $out/share/applications/${chooser}.desktop <<EOF
      [Desktop Entry]
      Name=${chooser}
      Exec=$out/bin/${chooser}
      Type=Application
      EOF
    '')
    [
      "audio-chooser"
      "internet-chooser"
      "display-chooser"
    ]
  }
''
