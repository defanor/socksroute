PROG=socksroute

all: build

sandbox:
	test -d .cabal-sandbox || cabal sandbox init

build: sandbox
	cabal update
	cabal install

install-files:
	cp .cabal-sandbox/bin/$(PROG) /usr/local/bin/
	cp $(PROG).service /usr/lib/systemd/system/
	id -u $(PROG) || useradd $(PROG)
	systemctl enable $(PROG)
	systemctl start $(PROG)

uninstall:
	systemctl stop $(PROG)
	systemctl disable $(PROG)
	rm -f /usr/local/bin/$(PROG) /usr/lib/systemd/system/$(PROG).service

install: build install-files
