all:
	(cd lib; $(MAKE))
	(cd src; $(MAKE))

clean:
	(cd lib; $(MAKE) clean)
	(cd src; $(MAKE) clean)

lib:
	(cd lib; $(MAKE))
