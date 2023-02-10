######################################################################
# A simple makefile to generate notes as epub/pdf/html/plaintext
# it requires gnumakefile, a shell, grep and pandoc (with pdf support)
######################################################################
BUILD_DIR ?= _build/notes
NOTES_DIR ?= notes
NOTES = $(shell ls $(NOTES_DIR) | grep -E "^[0-9]+")

# template to generate the targets
define pandoc_template =
NOTES_TARGETS += $$(BUILD_DIR)/$(1).pdf
$$(BUILD_DIR)/$(1).pdf:
	pandoc -f markdown -t pdf -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).epub
$$(BUILD_DIR)/$(1).epub:
	pandoc -f markdown -t epub -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).txt
$$(BUILD_DIR)/$(1).txt:
	pandoc -f markdown -t plain -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"

NOTES_TARGETS += $$(BUILD_DIR)/$(1).html
$$(BUILD_DIR)/$(1).html:
	pandoc -f markdown -t html -o $$@ \
		--resource-path="$$(NOTES_DIR)/$(1)" \
		"$$(NOTES_DIR)/$(1)/README.md"
endef

.PHONY += all
all: notes

$(BUILD_DIR):
	mkdir -p $@

# generate all templates based on notes directory name
$(foreach note,$(NOTES),$(eval $(call pandoc_template,$(note))))

.PHONY += notes
notes: $(BUILD_DIR) $(NOTES_TARGETS)

.PHONY: $(.PHONY)
