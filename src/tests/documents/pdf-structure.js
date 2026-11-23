// The structure tree of a tagged PDF, as an indented list of its elements
// (with the Alt of the formulas and pictures), their marked contents and
// their annotations (docs/pdf-tagging.md).
//
// usage: mutool run tests/documents/pdf-structure.js file.pdf
var pdf = Document.openDocument(scriptArgs[0]);
var root = pdf.getTrailer().get("Root").get("StructTreeRoot");
function has(o, k) { var v = o.get(k); return v && !v.isNull(); }
function show(o, ind) {
  if (o.isDictionary()) {
    var t = o.get("Type");
    if (t && t.toString() == "/MCR") { print(ind + "mcid " + o.get("MCID")); return; }
    if (t && t.toString() == "/OBJR") {
      print(ind + "annotation " + o.get("Obj").get("Subtype")); return; }
    var s = o.get("S"), alt = o.get("Alt"), a = o.get("A");
    var line = ind + (s ? s.toString() : "?");
    if (alt && !alt.isNull()) line += "  alt=" + alt.asString();
    if (a && a.isDictionary()) {
      if (has(a, "ColSpan")) line += "  colspan=" + a.get("ColSpan");
      if (has(a, "RowSpan")) line += "  rowspan=" + a.get("RowSpan");
    }
    print(line);
    var k = o.get("K");
    if (!k) return;
    if (k.isArray()) for (var i = 0; i < k.length; i++) show(k.get(i), ind + "  ");
    else if (!k.isNull()) show(k, ind + "  ");
  } else if (o.isNumber()) print(ind + "mcid " + o);
}
if (root.isNull()) print("no structure tree: the PDF is not tagged");
else show(root.get("K"), "");
