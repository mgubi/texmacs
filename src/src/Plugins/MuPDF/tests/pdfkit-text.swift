// The text of the pages of a PDF as PDFKit (Preview) gives it, which takes
// it from the character maps of the fonts and not from the ActualText of
// the spans:
//
//   swift src/Plugins/MuPDF/tests/pdfkit-text.swift <file.pdf>
import PDFKit
let doc = PDFDocument (url: URL (fileURLWithPath: CommandLine.arguments[1]))!
for i in 0 ..< doc.pageCount { print (doc.page (at: i)!.string ?? "") }
