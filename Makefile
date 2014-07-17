all:
	corebuild main.native -pkg cohttp.async -pkg ezjsonm &&\
	mv main.native server

clean:
	corebuild -clean
