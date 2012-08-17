other read X { // Calls tl
	A := hd X;
	B := tl X;
} write B

main read X { 
	A := hd X;
	B := tl X;
	Z := [other](B);  // = tl B
	Z := cons A Z;    // = cons (hd X) (tl tl X)
} write Z
