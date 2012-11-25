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
	  Y := (nil.(hd Initial).(tl Initial).nil);
  } else {
	  Y := nil.nil.nil.nil;
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
	Right := (hd Tape); Tape := tl Tape;
	if [not](End = State) {
		Ret := [lookup](Transition.(Current.State));
		if [not](Ret) { Error := :no_connection; X := tl nil; }

		Current := hd Ret; Ret := tl Ret;
		State   := hd Ret; Ret := tl Ret;
		Dir     := hd Ret; Ret := tl Ret;
		if (Dir = :R) {
			Left := cons Current Left;
			if Right {
				Current := hd Right;
				Right := tl Right;
			} else {
				Current := nil;
			}
		}
		if (Dir = :L) {
			Right := cons Current Right;
			if Left {
				Current := hd Left;
				Left := tl Left;
			} else {
				Current := nil;
			}
		}
  }

	Tape := (Left.Current.Right.nil);
	TM := (Tape.State.Transition.End.nil);
} write TM

add_to_map read MAPXY {
	MAP := hd MAPXY; MAPXY := tl MAPXY;
	X := hd MAPXY; MAPXY := tl MAPXY;
	Y := hd MAPXY;
	MAP := cons (cons X Y) MAP;
} write MAP

any read LIST {
	for E in LIST {
		if (E) {
			B := cons nil nil;
		}
	}
} write B

linear_tape read TAPE {
	L := hd TAPE; TAPE := tl TAPE;
	N := hd TAPE; TAPE := tl TAPE;
	R := hd TAPE; TAPE := tl TAPE;
  R := cons N R;
	while R {
		h := hd R;
		R := tl R;
		if [not]([any](R)) {
			R := nil;
		}
		RRev := cons h R;
	}
	for X in RRev {
		R := cons X R;
	}
	for X in L {
		R := cons X R;
	}
	LINEARTAPE := R;
} write LINEARTAPE

run_tm read TM {
	NTM := [run_round](TM);
	while [not](TM = NTM) {
		TM := NTM;
		NTM := [run_round](TM);
	}
} write TM

example read X {
	TAPE := [make_band]((:bla).(:bla).(:bla).(:bla).(:bla).(:bla).nil);
	MAP := [add_to_map](nil.((:bla).(:even)).((:o).(:odd).(:R).nil).nil);
	MAP := [add_to_map](MAP.((:bla).(:odd)).((:o).(:even).(:R).nil).nil);
	MAP := [add_to_map](MAP.(nil.(:odd)).(nil.(:odd_del).(:L).nil).nil);
	MAP := [add_to_map](MAP.(nil.(:even)).(nil.(:even_del).(:L).nil).nil);
	MAP := [add_to_map](MAP.(nil.(:odd_del)).((:odd).(:end).(:N).nil).nil);
	MAP := [add_to_map](MAP.(nil.(:even_del)).((:even).(:end).(:N).nil).nil);
	MAP := [add_to_map](MAP.((:o).(:odd_del)).(nil.(:odd_del).(:L).nil).nil);
	MAP := [add_to_map](MAP.((:o).(:even_del)).(nil.(:even_del).(:L).nil).nil);
	STATE := (:even);
	END := (:end);
	TM := TAPE.STATE.MAP.END.nil;
	TM := [run_tm](TM);
	LINEARTAPE := [linear_tape](hd TM);
} write LINEARTAPE
