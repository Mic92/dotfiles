{ pamixer, kdialog, pulseaudio, runCommand, lib, runtimeShell }:
runCommand "audio-chooser" { } ''
  mkdir -p $out/bin $out/share/applications
  cat > $out/bin/audio-chooser <<'EOF'
  #!${runtimeShell}
  export PATH=${lib.makeBinPath [ pamixer kdialog pulseaudio ]}:$PATH
  ${builtins.readFile ./audio-chooser.sh}
  EOF
  chmod +x $out/bin/audio-chooser
  cat > $out/share/applications/audio-chooser.desktop <<EOF
  [Desktop Entry]
  Name=audio-chooser
  Exec=$out/bin/audio-chooser
  Type=Application
  Categories=AudioVideo;Audio;Mixer;
  EOF
''

