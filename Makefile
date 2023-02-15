version:
	emacs --version

test : version
	cask exec emacs --script test/ejc-tests.el

elpa:
ifeq ($(OS),Windows_NT)
	if exist $(USERPROFILE)\.cask rmdir $(USERPROFILE)\.cask /s /q
	git clone https://github.com/cask/cask $(USERPROFILE)\.cask
	make -C $(USERPROFILE)\.cask cask install
else ifeq ($(wildcard $(HOME)/.cask/bin/),)
	rm -rf ~/.cask
	git clone https://github.com/cask/cask $(HOME)/.cask
	make -C ~/.cask cask install
endif
	cask install
