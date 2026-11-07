{
  emacs-nox,
  writeShellApplication,
}:
# There is no maintained standalone Common Lisp formatter. Emacs'
# cl-indent is what SLIME and Sly use, so it is the reference.
writeShellApplication {
  name = "lisp-indent";
  runtimeInputs = [ emacs-nox ];
  text = ''
    exec emacs --batch --quick --load ${./indent.el} "$@" 2>/dev/null
  '';
}
