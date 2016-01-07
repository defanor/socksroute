PROG=socksroute

all: build

sandbox:
	test -d .cabal-sandbox || cabal sandbox init

build: sandbox
	cabal update
	cabal install

install-files:
	sudo cp .cabal-sandbox/bin/$(PROG) /usr/local/bin/
	sudo cp $(PROG).service /usr/lib/systemd/system/
	id -u $(PROG) || sudo useradd $(PROG)
	sudo systemctl enable $(PROG)
	sudo systemctl start $(PROG)

uninstall:
	sudo systemctl stop $(PROG)
	sudo systemctl disable $(PROG)
	sudo rm -f /usr/local/bin/$(PROG) /usr/lib/systemd/system/$(PROG).service

install: build install-files
