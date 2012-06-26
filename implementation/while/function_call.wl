/* Tests multiple functions */
not read X {
	if X {
		Y := nil
	}
	else {
		Y := cons nil nil
	}
} write Y

and read X {
	A := hd X
	B := tl X
	if [not](B) {
		A := B
	}
} write A

equals read X {
	A := hd X
	B := tl X
	Z := cons nil nil
	while [and](cons A B) {
		if Z {
			if [not]([equals](cons (hd A) (hd B))) {
				Z := nil
			}
		}
		A := tl A
		B := tl B
	}
} write Z
