.pragma library
// Pure text-munging helpers. .pragma library = stateless singleton.

function snippet(text, n) {
  const t = (text || "").replace(/\s+/g, " ").trim();
  return t.length > n ? t.slice(0, n - 1) + "…" : t;
}

// QML color → #rrggbb. Qt stringifies as #aarrggbb but <font>/<span>
// only parse the 6-digit form.
function hex6(c) {
  return String(c).replace(/^#(..)(......)$/, "#$2");
}

// Qt's MarkdownText pulls link colour from the global QPalette::Link
// role. TextEdit isn't a Control so a local palette override doesn't
// reach it. Rewrite [label](url) → <a><font color=…> — CommonMark
// passes inline HTML through and <font> is the one tag RichText
// reliably honours for colour.
function colorizeLinks(md, c) {
  const h = hex6(c);
  return (md || "").replace(
    /\[([^\]]+)\]\(([^)\s]+)\)/g,
    (_, label, url) =>
      `<a href="${url}"><font color="${h}">${label}</font></a>`);
}

// Wrap case-insensitive matches in a highlight span. Carves out code
// spans and link targets — the two regions where injected HTML does
// damage (renders literally / breaks the href). Everything else is
// fair game; a mangled **bold** while search is open is cosmetic.
function highlight(md, q, bg, fg) {
  if (!q) return md;
  const esc = q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const re = new RegExp(esc, "gi");
  const b = hex6(bg), f = hex6(fg);
  const wrap = m => `<span style="background-color:${b};color:${f}">${m}</span>`;
  return (md || "")
    .split(/(```[\s\S]*?```|`[^`\n]*`|\]\([^)\s]*\))/g)
    .map((seg, i) => i & 1 ? seg : seg.replace(re, wrap))
    .join("");
}
