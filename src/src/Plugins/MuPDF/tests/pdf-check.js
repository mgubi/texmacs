// The structure of a PDF, as MuPDF checks it (PDFDocument.check, MuPDF
// 1.27 and later): the cross reference table and the syntax of the objects.
// Prints "ok", or what MuPDF had to say (it says it on stderr, which the
// caller merges). mutool run pdf-check.js file.pdf
var doc= Document.openDocument (scriptArgs[0]).asPDF ();
if (typeof doc.check == "function") doc.check ();
print (doc.wasRepaired () ? "repaired" : "ok");
