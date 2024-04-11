TL=teal/tl
SRC_DIR=src
BUILD_DIR=build

# will be transpile to lua with $(TL), ignore .d.tl files
TEAL_FILES=$(shell find $(SRC_DIR) -name "*.tl" -not -name "*.d.tl")
# will be copied to $(BUILD_DIR)
LUA_FILES=$(shell find $(SRC_DIR) -name "*.lua")

TEAL_OUT=$(patsubst $(SRC_DIR)/%.tl, $(BUILD_DIR)/%.lua, $(TEAL_FILES))
LUA_OUT=$(patsubst $(SRC_DIR)/%.lua, $(BUILD_DIR)/%.lua, $(LUA_FILES))

.PHONY: all clean install

all: $(TEAL_OUT) $(LUA_OUT)

$(BUILD_DIR)/%.lua: $(SRC_DIR)/%.tl
	@echo "Transpiling $< to $@"
	@mkdir -p $(@D)
	$(TL) gen $< -o $@

$(BUILD_DIR)/%.lua: $(SRC_DIR)/%.lua
	@echo "Copying $< to $@"
	@mkdir -p $(@D)
	cp $< $@

clean:
	rm -rf $(BUILD_DIR)

install:
	@echo "installing main.lua to $(INST_BINDIR)"
	@mkdir -p $(INST_BINDIR)
	install -m 755 $(BUILD_DIR)/main.lua $(INST_BINDIR)/teal-compiler
	@echo "Copying modules to $(INST_LUADIR)"
# Copy all out files to this dir
	cp -r $(BUILD_DIR)/* $(INST_LUADIR)
	@echo "Done"

