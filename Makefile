SUBDIRS = implementation text presentation
subdirs:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir; \
	done

.PHONY: subdirs 
