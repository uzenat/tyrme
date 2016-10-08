EXECUTABLE=tyrme

$(EXECUTABLE): 
	cd SOURCE &&  make 
	mv SOURCE/tyrme tyrme

clean:
	cd SOURCE && make clean
	rm -f *~

cleanall:
	make clean
	rm -f tyrme