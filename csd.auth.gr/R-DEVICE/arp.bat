"c:\Program Files\Libwww\loadtofile.exe" %1 -o %2.rdf
java -cp "c:\Program Files\arp\arp.jar;c:\Program Files\xerces\xerces.jar" com.hp.hpl.jena.rdf.arp.NTriple %1 > %2.n3
rem pause
