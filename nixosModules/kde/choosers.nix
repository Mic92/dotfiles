{
  pamixer,
  fuzzel,
  pulseaudio,
  runCommand,
  lib,
  runtimeShell,
  bluez,
  libnotify,
  python3,
}:
runCommand "audio-chooser" { } ''
  mkdir -p $out/bin $out/share/applications

  # Audio chooser (Python)
  cat > $out/bin/audio-chooser <<'EOF'
  #!${python3}/bin/python3
  import sys
  import os
  os.environ['PATH'] = '${
    lib.makeBinPath [
      pamixer
      fuzzel
      pulseaudio
      bluez
      libnotify
    ]
  }:' + os.environ.get('PATH', "")
  ${builtins.readFile ./audio_chooser.py}
  EOF
  chmod +x $out/bin/audio-chooser
  cat > $out/share/applications/audio-chooser.desktop <<EOF
  [Desktop Entry]
  Name=audio-chooser
  Exec=$out/bin/audio-chooser
  Type=Application
  EOF

  # Other choosers (shell scripts)
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
      "internet-chooser"
      "display-chooser"
    ]
  }
''
