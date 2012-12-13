not read X {
	if X {
		Y := nil;
	} else {
		Y := nil.nil;
	}
} write Y 

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

literal read X {
	if [not](X) {
		Y := (:nilexp);
	} else {
		if (atom? X) {
			Y := (:symbol).X;
		} else {
			head := [literal](hd X);
			tail := [literal](tl X);
			Y := (:consexp).head.tail;
		}
	}
} write Y

spec read PX {
	P := hd PX; X := (tl PX);
	name := hd (tl P);
	i    := hd (tl (tl P));
	o    := hd (tl (tl (tl P)));
	b    := hd (tl (tl (tl (tl P))));
	exp  := (:consexp).((:literal).X.nil).((:var).i.nil).nil;
	before := (:Assign).i.exp.nil;
	b    := cons before b;
	Q := (:program).name.i.o.b.nil;
} write Q

iterate_combinator read CPX {
	Combinator := hd CPX; CPX := tl CPX;
	Program    := hd CPX; CPX := tl CPX;
	X          := CPX;
	Y := [[ Program ]]([spec](Combinator.(Combinator.Program)).X);
} write Y

Y read F {
	ic := <iterate_combinator>;
	Prog := [spec](ic.ic);
	Prog := [spec](Prog.F);
} write Prog

id read X {
	X := X;
} write X


main read X {
	ide := <id>;
	ide := [Y](ide);
	is_quine := ide = [[ide]](nil);
} write ide
