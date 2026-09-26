// a figure with a layer which is off by default: a black frame, always
// seen, and a red square in the layer "Hidden", which must not be seen
var doc= new PDFDocument ();
var ocg= doc.addObject ({ Type: "OCG", Name: "Hidden" });
var res= doc.addObject ({ Properties: { L1: ocg } });
var contents= "0 0 0 RG 4 w 10 10 180 80 re S\n" +
              "/OC /L1 BDC 1 0 0 rg 60 20 80 60 re f EMC\n";
var page= doc.addPage ([0, 0, 200, 100], 0, res, contents);
doc.insertPage (-1, page);
doc.getTrailer ().Root.OCProperties= doc.newDictionary ();
var ocp= doc.getTrailer ().Root.OCProperties;
ocp.OCGs= doc.newArray (); ocp.OCGs.push (ocg);
ocp.D= doc.newDictionary ();
ocp.D.OFF= doc.newArray (); ocp.D.OFF.push (ocg);
ocp.D.Order= doc.newArray (); ocp.D.Order.push (ocg);
doc.save (scriptArgs[0]);
