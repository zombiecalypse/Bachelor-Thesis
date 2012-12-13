extractProgramName read P {
	Y := hd (tl P);
} write Y

setProgramName read PN {
	P := hd PN; N := tl PN;
	Pp := (:program).N.(tl (tl P));
} write Pp

extractBlock read P {
	Y := hd (tl (tl (tl (tl P))));
} write Y
	

main read X {
	self := <main>;
	self := [[ <setProgramName> ]](self.(:other));
	self := [extractBlock](self);
	statement := hd (tl self);
} write statement
