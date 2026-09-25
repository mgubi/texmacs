// The structure of a PDF, for pdf-compare.sh: outline, named destinations,
// links, metadata. mutool run structure.js file.pdf
// the structure of a PDF: outline, named destinations, links, metadata
var doc= Document.openDocument (scriptArgs[0]);
function outline (list, depth) {
  if (!list) return;
  for (var i=0; i<list.length; i++) {
    var o= list[i];
    var ind= ""; for (var k=0; k<depth; k++) ind += "  "; print (ind + "outline: " + o.title + " -> " + o.uri + (o.open ? " open" : " folded"));
    outline (o.down, depth + 1);
  }
}
outline (doc.loadOutline (), 0);
var pdf= doc.asPDF ();
var pagenum= {};
for (var p=0; p<pdf.countPages (); p++) pagenum[pdf.findPage (p).asIndirect ()]= p;
var names= pdf.getTrailer ().get ("Root").get ("Names");
if (names && names.get ("Dests") && names.get ("Dests").get ("Names")) {
  var n= names.get ("Dests").get ("Names");
  for (var i=0; i<n.length; i+=2) {
    var d= n.get (i+1).resolve ();
    print ("dest: " + n.get (i).asString () + " -> page " + pagenum[d.get (0).asIndirect ()] + " " + d.get (1) + " " + Math.round (d.get (2).valueOf ()) + " " + Math.round (d.get (3).valueOf ()));
  }
}
for (var p=0; p<doc.countPages (); p++) {
  var links= doc.loadPage (p).getLinks ();
  for (var i=0; i<links.length; i++) print ("link p" + p + ": " + links[i].getURI () + " " + links[i].getBounds ().map (function (x) { return Math.round (x); }).join (","));
}
print ("title: " + doc.getMetaData ("info:Title"));
print ("author: " + doc.getMetaData ("info:Author"));
print ("producer: " + doc.getMetaData ("info:Producer"));
print ("pages: " + doc.countPages ());
