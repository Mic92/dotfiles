{ pkgs, ... }: {
  home.packages = with pkgs; [
    # latex
    rubber
    (texlive.combine {
      inherit (texlive)
      scheme-full

      # awesome cv
      xetex
      unicode-math
      ucharcat
      collection-fontsextra
      fontspec;

      #collection-binextra
      #collection-fontsrecommended
      #collection-latex
      #collection-latexextra
      #collection-latexrecommended
      #collection-langgerman
      #siunitx
      #bibtex
      #tracklang
      #IEEEtran
      #algorithm2e;
    })
  ];
}
