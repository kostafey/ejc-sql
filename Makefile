version:
	emacs --version

test : version
	cask exec emacs --script test/ejc-tests.el

elpa:
	cask install
	cask update
