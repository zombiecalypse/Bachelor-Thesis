extractProgramName read P {
	Y := hd (tl P);
} write Y

setProgramName read PN {
	P := hd PN; N := tl PN;
	Pp := (:program).N.(tl (tl P));
} write Pp
	

main read X {
	self := <main>;
	self := [[ <setProgramName> ]](self.(:other));
	self := [extractProgramName](self);
} write self
