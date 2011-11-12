
platform:=$(shell uname)

install: submodules-update
ifeq ($(findstring CYGWIN,$(platform)),CYGWIN)
	@cmd /q /c tools\\junction -s -q "%HOME%\org" "%HOME%\Dropbox\org"
endif
ifeq ($(findstring Darwin,$(platform)),Darwin)
	$(shell ln -s ~/Dropbox/org ~/org)
endif

submodules-update:
	@git submodule update --init --recursive

submodules-pull:
	@git submodule foreach "git pull origin master"
	@git submodule status | awk '/\+/{print $0}' | cut -d ' ' -f 2 | \
	xargs -I module sh -c 'git add module; git commit -m "updated module"'
