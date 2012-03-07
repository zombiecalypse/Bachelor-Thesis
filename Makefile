text:
	echo "Building Text"
	cd text; $(MAKE) $(MFLAGS)
implementation:
	echo "Building Implementations"
	cd implementation; $(MAKE) $(MFLAGS)

.PHONY: text implementation
