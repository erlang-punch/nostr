######################################################################
# A simple makefile to generate notes as epub/pdf/html/plaintext
# it requires gnumakefile, a shell, grep and pandoc (with pdf support)
######################################################################
BUILD_DIR ?= _build/notes
NOTES_DIR ?= notes
NOTES = $(shell ls $(NOTES_DIR) | grep -E "^[0-9]+-")
PANDOC_OPTS = -C
PANDOC = pandoc $(PANDOC_OPTS)

######################################################################
# template to generate the targets
######################################################################
define pandoc_template =
NOTES_TARGETS += $$(BUILD_DIR)/$(1).pdf
$$(BUILD_DIR)/$(1).pdf:
	$(PANDOC) -f markdown -t pdf -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).epub
$$(BUILD_DIR)/$(1).epub:
	$(PANDOC) -f markdown -t epub -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).txt
$$(BUILD_DIR)/$(1).txt:
	$(PANDOC) -f markdown -t plain -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).html
$$(BUILD_DIR)/$(1).html:
	$(PANDOC) -f markdown -t html -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"
endef

######################################################################
# default target, used to build automatically the notes
######################################################################
.PHONY += all
all: notes

######################################################################
# create the build directory
######################################################################
$(BUILD_DIR):
	mkdir -p $@

######################################################################
# generate all templates based on notes directory name
######################################################################
$(foreach note,$(NOTES),$(eval $(call pandoc_template,$(note))))

######################################################################
# generate all notes
######################################################################
.PHONY += notes
notes: $(BUILD_DIR) $(NOTES_TARGETS)

######################################################################
# remove all generated articles and notes
######################################################################
.PHONY += clean
clean:
	rm $(NOTES_TARGETS)

######################################################################
# usage
######################################################################
help:
	@echo "Usage: make [help|all|notes|clean]"

######################################################################
# .PHONY target
######################################################################
.PHONY: $(.PHONY)
