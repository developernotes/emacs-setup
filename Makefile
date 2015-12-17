
platform:=$(shell uname)

install:
ifeq ($(findstring CYGWIN,$(platform)),CYGWIN)
	@cmd /q /c tools\\junction -s -q "%HOME%\org" "%HOME%\Dropbox\org"
endif
ifeq ($(findstring Darwin,$(platform)),Darwin)
	$(shell ln -s ~/Dropbox/org ~/org)
endif
