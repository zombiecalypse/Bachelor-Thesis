not read X {
	if X {
		Y := nil;
	}
	else {
		Y := cons nil nil;
	}
} write Y

make_band read Initial {
	if Initial {
	  Y := cons (cons nil nil) (cons (hd Initial) (cons (cons (tl Initial) nil) nil));
  } else {
	  Y := cons (cons nil nil) (cons nil (cons (cons nil nil) nil));
  }
} write Y

lookup read MAPX {
	Map := hd MAPX;
	X   := tl MAPX;
	for I in Map {
		if (hd I = X) {
			Y := tl I;
		}
	}
} write Y

run_round read TM { // ./eval-while examples/turing.wl  -i'(nil.(:x).nil.nil).(:bla).((((:x).(:bla)).((:x).(:bla)).nil)).(:bla).nil' --list
	Tape       := hd TM; TM := tl TM;
	State      := hd TM; TM := tl TM;
	Transition := hd TM; TM := tl TM;
	End        := hd TM; TM := tl TM;

	Left := hd Tape; Tape := tl Tape;
	Current := hd Tape; Tape := tl Tape;
	Right := hd Tape; Tape := tl Tape;
	if [not](End = State) {
		Ret := [lookup](Transition.(Current.State));
		if [not](Ret) { Error := :no_connection; X := tl nil; }

		Current := hd Ret; Ret := tl Ret;
		State   := hd Ret; Ret := tl Ret;
		Dir     := hd Ret; Ret := tl Ret;
  }

	Tape := (Left.Current.Right.nil);
	TM := (Tape.State.Transition.End.nil);
} write TM
