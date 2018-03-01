
PHONY=all
RM=rm -f

all: adataptempo

adataptempo:	messages.stamp $(wildcard src/*.ad?)
	gprbuild

messages.stamp:	$(wildcard resources/taptempo*.properties)
	cd src && \
	zbmcompile -a -O -i -v -d ../resources -S messages.stamp TapTempo_Messages taptempo && \
	zbmcompile -a -O -i -v -d ../resources -S messages.stamp Options_Messages options

clean:
	gprclean
	-$(RM) src/messages.stamp src/*_messages*.ad[sb]
