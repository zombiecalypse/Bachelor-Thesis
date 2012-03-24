length read x1 {
	x2 := nil
	while x1 {
		x2 := cons nil x2
		x1 := tl x1
	}
}
write x1
